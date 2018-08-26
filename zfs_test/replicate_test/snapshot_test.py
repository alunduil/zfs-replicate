from hypothesis import given


@given(lists(snapshots()))
def test_snapshots(snapshots):
    output = "\n".join([_output(s) for s in snapshots])
    assert _snapshots(output) == snapshots

@given(snapshots())
def test_snapshot(snapshot):
    assert _snapshot(_output(snapshot)) == snapshot

snapshots = tuples(text(), text(), datetime()).map(Snapshot)

def _output(snapshot: Snapshot) -> str():
    return "{snapshot.dataset}@{snapshot.name}\t{snapshot.timestamp}".format(snapshot)
