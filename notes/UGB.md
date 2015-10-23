# Input types

|Input      |Key           |Value                            |NumChannels|
|-----------|--------------|---------------------------------|-----------|
|`Action`   |`AttributeKey`|`Action.Value.type` (like `Unit`)|N          |
|`Attribute`|`AttributeKey`|`Attribute.Value`                |Y          |
|`Buffer`   |`AttributeKey`|`Buffer.Value`                   |Y          |
|`DiskOut`  |`AttributeKey`|`DiskOut.Value`                  |Y          |
|`Stream`   |`AttributeKey`|`Stream.Value`                   |Y          |

So in base SoundProcesses (after removal of `proc.inputs`), we only ever deal 
with inputs that use attribute-keys, all of
which have a value determining the number of channels (except `Action`).

SysSon however adds a series of new keys, e.g. `Dim.Size`, `Elapsed`, so
we should not flatten the `Key` to `AttributeKey`.

# Tracing build process

After each `tryBuild` we look at the

- newly rejected inputs -- watch attribute keys and `tryBuild` if they change, appear or disappear
- newly accepted inputs -- watch attribute keys and re-check   if they change, appear or disappear.
  If change is incompatible, rebuild from zero. If change is compatible and synth is running, 
  run node updates
- newly accepted outputs -- intersect with declared outputs -- for those in the intersection,
  'publish' number of output channels
  
Note that rejected-inputs can only shrink (except for initial build) and accepted-inputs can
only grow.

What happens with an initial mismatch in the number-of-channels. E.g. input request specifies
`i` channels and actual attribute has `j != i` channels? That currently results in a generic
exception, not `MissingIn`.

It would make sense to treat that like a `MissingIn`, because technically it means there is
no usable input attribute available.

We should still distinguish `ScanIn` and `Attribute` in the sense that the former requires an
attribute to be present, whereas the latter will use its default value when the attribute is
missing.


