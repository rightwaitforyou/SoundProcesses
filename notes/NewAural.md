- for each scan we create an AuralScan
- we have to maintain an asymmetry between sinks and sources to simply connectivity
- let's say, the AuralScan contacts the sinks

# Transport

Macht eine Entkopplung von Transport/Timeline Sinn
(Entkopplung ja, aber die Vorstellung, dass ein Transport
etwas anderes transportieren koennte?)

Wie gehen verschachtelte Timelines?

    Transport {
      def apply(view: AuralTimeline) = ???
    }

-----

andererseits soll transport auch ein modell sein, dass rein visuell funktioniert, d.h. in TimelineView ohne aural system

    Transport {
      def apply() = ???
    }

    trait AuralObj {
      def play()(implicit mt: MaybeTransport)
    }

versus

    object AuralObj {
      def apply(obj: Obj)(implicit mt: MaybeTransport)
    }

womit `mt` mit AuralContext verschwimmen wuerde

    object AuralContext {
      def apply(mt: Option[Transport])
    }

Es soll aber moeglich sein, procs ueber timeline grenzen hinweg zu verschalten, das hiesse der aural-context waere derselben, und damit muesste er unabhaengig vom transport sein.

AuralContext koennte ein Wallclock Time signal maintainen, das einfach immer weiter laeuft. Bei Timeline.SampleRate -- kann das Ding Wochen lang laufen ohne Overflow? (JA! 20 Tsd. Jahre)

Das macht Sinn, weil es eine 1:1 Beziehung AuralContext -- Server gibt; d.h. ultimately koennte auch audio-clock zu Grunde liegen

Dann koennte der AuralContext genutzt werden, client-side timing zu machen, egal ob es einen "Transport" gibt oder nicht; etwa re grapheme playing

Transport waere dann nur eine relative frame position. Bei transport-play wird die frame position logisch mit der aktuellen aural-context wallclock verknuepft.

Dann waere es sicher interessanter und genereller, bei verschachtelten Timelines davon auszugehen, dass jede ihren eigenen Transport hat? Synchronisieren kann man die immer noch. Eine einfache und flexible Loesung waere, ein attribute der inneren timeline als start position zu nehmen (bzw. wenn nicht gesetzt, sind beide synchronisiert).

Es waere auch gut, ueber `thisTransport` nachzudenken bzw. es zu ermoeglichen ein solches DSL objekt irgendwann einfach zu implementieren.

Persistenz: Soll timeline cursor im modell oder nur view gespeichert werden? Nachteil von ersterem waere wohl, dass ziemlich viele overwrites zu erwarten sind? (axis-drag)


