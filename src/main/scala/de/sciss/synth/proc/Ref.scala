/*
 *  Ref.scala
 *  (SoundProcesses)
 *
 *  Copyright (c) 2010-2012 Hanns Holger Rutz. All rights reserved.
 *
 *  This software is free software; you can redistribute it and/or
 *  modify it under the terms of the GNU General Public License
 *  as published by the Free Software Foundation; either
 *  version 2, june 1991 of the License, or (at your option) any later version.
 *
 *  This software is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
 *  General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public
 *  License (gpl.txt) along with this software; if not, write to the Free Software
 *  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
 *
 *
 *  For further information, please contact Hanns Holger Rutz at
 *  contact@sciss.de
 */

package de.sciss.synth.proc

import concurrent.stm.{Txn, TxnLocal, InTxn, Ref => ScalaRef}

object Ref {
   /**
    *    Creates a Ref whose value is checked before commit using the provided
    *    <code>PartialFunction</code>. If the function is defined at the particular
    *    value (the new value as changed during the transaction), the function
    *    is evaluated after the commit. Therefore clean-up behaviour such as
    *    freeing resources and other side-effect actions can take place.
    */
   def withCheck[ @specialized T ]( initialValue: T )( cleanUp: PartialFunction[ T, Unit ])
                              ( implicit m: ClassManifest[ T ]) : ScalaRef[ T ] =
      new CleanUpImpl( ScalaRef( initialValue ), cleanUp )

   private sealed trait Proxy[ T ] extends ScalaRef[ T ] {
      protected def peer: ScalaRef[ T ]
      def get( implicit tx: InTxn ) : T  = peer.get
      def set( v: T )( implicit tx: InTxn ) { peer.set( v )}
      def swap( v: T )( implicit tx: InTxn ) : T = peer.swap( v )
      def transform( f: T => T )( implicit tx: InTxn ) { peer.transform( f )}
      def transformIfDefined( pf: PartialFunction[ T, T ])( implicit tx: InTxn ) : Boolean = peer.transformIfDefined( pf )
      def relaxedGet( equiv: (T, T) => Boolean )(implicit tx: InTxn ) : T = peer.relaxedGet( equiv )
      def trySet( v: T )( implicit tx: InTxn ) : Boolean = peer.trySet( v )
      def single : ScalaRef.View[ T ] = peer.single
      def getWith[ Z ]( f: T => Z )( implicit tx: InTxn ) : Z = peer.getWith( f )

//      def +=( rhs: T )( implicit tx: InTxn, num: Numeric[ T ]) { peer.+=( rhs )}

      override def toString = peer.toString
   }

   private sealed trait TouchImpl[ T ] extends Proxy[ T ] {
      private val touchedRef = TxnLocal( false )

      private def touch( tx: InTxn ) {
         if( !touchedRef.swap( true )( tx )) {
            touched( tx )
         }
      }

      protected def touched( tx: InTxn ) : Unit

      override def set( v: T )( implicit tx: InTxn ) { touch( tx ); super.set( v )}
      override def swap( v: T )( implicit tx: InTxn ) : T = { touch( tx ); super.swap( v )}
      override def transform( f: T => T )( implicit tx: InTxn ) { touch( tx ); super.transform( f )}
      override def transformIfDefined( pf: PartialFunction[ T, T ])( implicit tx: InTxn ) : Boolean = {
         val res = super.transformIfDefined( pf )
         if( res ) touch( tx )
         res
      }
      override def trySet( v: T )( implicit tx: InTxn ) : Boolean = {
         touch( tx ); super.trySet( v )
      }
      override def *=( v: T )( implicit tx: InTxn, num: Numeric[ T ]) { touch( tx ); super.*=( v )}
      override def +=( v: T )( implicit tx: InTxn, num: Numeric[ T ]) { touch( tx ); super.+=( v )}
      override def -=( v: T )( implicit tx: InTxn, num: Numeric[ T ]) { touch( tx ); super.-=( v )}
      override def /=( v: T )( implicit tx: InTxn, num: Numeric[ T ]) { touch( tx ); super./=( v )}
      override def update( v: T )( implicit tx: InTxn ) { touch( tx ); super.update( v )}

      override def single : ScalaRef.View[ T ] = sys.error( "Unsupported operation : single" )
   }

   private final class CleanUpImpl[ T ]( protected val peer: ScalaRef[ T ], fun: PartialFunction[ T, Unit ])
   extends TouchImpl[ T ] {
      protected def touched( tx: InTxn ) {
         Txn.beforeCommit( implicit t0 => {
            val newValue = peer.get
            if( fun.isDefinedAt( newValue )) Txn.afterCommit( _ => fun( newValue ))( t0 )
         })( tx )
      }
   }
}
