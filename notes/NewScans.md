    trait Proc {
      def inputs : Inputs
      def outputs: Outputs
    }
    
    // so having a single Scan value is one of multiple possibilities
    // - what's the advantage over just using the object's `attr` map then at all?
    type Inputs  = Obj.AttrMap
    
    type Outputs = Set[String]
    
There are two mixing situations -- multiple outputs go into one input,
and one output diverges to multiple inputs. The latter is irrelevant now,
but we need a new type reflects to first situation. The problem is that
values in the `BiPin` / `BiGroup` logic are single elements or immutable
collections. What is better, `BiPin[List]` or `BiGroup[Obj]`?
`BiPin[List]` doesn't make sense, we cannot represent the addition or
removal of links. So does the additional support of a `BiPin` attribute value
buy us anything? Apparently _not_.

So the logic would be to primarily support `BiGroup[Obj]` aka `Timeline`.
And that actually looks elegant, being able to feed a timeline into an
`Attribute.ar`. The only difference then is that a "normal" timeline
doesn't have a concept of a common output signal, whereas here we need
that concept. So here instead of placing `proc` objects on the timeline,
we place `scan` objects. And hence, we will have different aural views
or equivalents of aural views. In the normal case, placing an `IntObj` or
`DoubleObj` does not result in any aural view, in the attr case it does.

For the case where no tP is maintained, to be able to connect (as before)
several outputs to one input (attribute), we would have to establish a
new type, such as `Set[Obj]` (or `List[Obj]` since currently `BiGroup`
has its value collection sequenced).

Eventually it would make sense to decentralise the attribute value views,
like through an `AuralAttributeValue`. Alternatively we can reuse `AuralView`
and supply the mix-bus through the `AuralContext`?

-------------------------------------

Request-Input

- Attribute     -- (lots)
- Stream        -- Grapheme.Expr.Audio
- Buffer        -- Grapheme.Expr.Audio
- Action        -- Action
- DiskOut       -- Artifact

In a first step, we could define an interface for attribute input support (the first one):

    object AuralAttribute {
      sealed trait Update[S <: Sys[S]]
      case class NumChannelsChanged[S <: Sys[S]](ai: AttributeInput[S], ch: Change[Int]) extends Update
    }
    // somehow similar to AuralObj
    trait AuralAttribute[S <: Sys[S]] extends Publisher[S, AttributeInput.Update[S]] with Disposable[S#Tx] {
      def numChannels: Int
      def accept()(implicit tx: S#Tx): Unit
      def prepare(builder: Builder, timeRef: TimeRef)(implicit tx: S#Tx): Unit
      def play   (builder: Builder, timeRef: TimeRef)(implicit tx: S#Tx): Unit
      def stop   ()(implicit tx: S#Tx): Unit
    }
    
    type Builder = ???
    
We have to different approaches:
 
 - setting a control directly on a node,
 - and playing to a bus.
 
Let's examine. Say there is `Folder` to which first an `IntObj` then an `Output` is added.
Say we change the presumed API to have these methods:

    def setControl(source: AuralAttribute, values: Vec[Float]): Unit
    ...
    
No.

    def realize(): AuralAttribute.Value
    
    sealed trait Value
    case class Scalar(values: Vec[Float]) extends Value
    case class Stream(???)                extends Value

We have to anticipate what we'll need for `AuralTimelineAttribute`. Do we want to dynamically
switch between node-setters and bus-mappers? Or do we want to require bus-mappers for a complex
object like that? I.e. assuming a `Nuages` session where virtually every parameter is on a
timeline but acting mostly compatible with a `BiPin`, do we really want the overhead to run
an extra synth per parameter? I think that _no_.

The receiving end must make sense of the value because the sending side might not
even know if there are other senders so that `setControl` won't work but needs addition/mixing.

Let's say to avoid complexity, as soon as we need to mix inputs, we'll use an auxiliary synth,
no client-side scalar addition or the like. Because that gets messy if elements are removed.

    scalar ---+
              |
    scalar ---+--- folder ---+
                             |
                   scalar ---+--- folder --- node-ref

Obviously we do only ever need a single bus.

                           +----------- TOPOLOGY-VERTEX ----------+
                           |                                      |
    scalar -> Control -> Out.ar ---+--- audio-bus -> mapa -> AudioControl
                                   |                              |
    scalar -> Control -> Out.ar ---+                              |
                            |      |                              |
    scalar -> Control -> Out.ar ---+                              |
                           ||                                     |
                           ++---------- TOPOLOGY-VERTEX ----------+

This is anyway a rare case. Usually if we have a mixing, that would be from multiple `Output` instances.

We could simplify again by saying that there is only ever a transition from node-setter to bus-mapper
but never back again (until of course disposal and rebuild). What makes it difficult is that the request
to "upgrade" to bus may need propagate in two directions (think of the first folder in the first diagram).
 
    (a) node-setter
    
    scalar ------- folder ------- folder --- node-ref

    (b) add another input - must switch to common bus
        and topology vertices
    
    scalar ---+
              |
    scalar ---+--- folder ------- folder --- node-ref

Why don't we abstract:

    trait AttributeTarget {
      def add(source: AnyRef, nodeRef: NodeRef, bus: lucre.synth.AudioBus)
      def add(source: AnyRef, scalar: Vec[Float])
      der remove(source: AnyRef)
    }
    
including the expanding and wrapping of the number of channels `bus.numChannels` / `scalar.size`. And there
is only one real target, and the intermediate folders or timeline instances simply forward.

Considering `Output` that might be written to different folders at the same time, we need to isolate
its own bus. That produces a problem above with `source: AnyRef`. Actually it doesn't. Because we have
the independent `AuralOutput` as singleton with multiple `AuralAttribute` relating to it.
