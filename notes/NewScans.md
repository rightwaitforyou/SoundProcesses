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
