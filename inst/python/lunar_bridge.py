"""Conversion helpers between lunapi and R (used by the luna R package).

These run on the Python side so that:

* Luna time-points (uint64 nanoseconds) reach R as float64 arrays, which
  hold them exactly for recordings under ~104 days.  Python ints above
  2^31 would otherwise be converted unreliably.
* Luna output tables are converted column by column to typed arrays,
  instead of pandas object columns, which reticulate turns into R
  list-columns.

Only the raw ``lunapi.lunapi0`` engine/instance methods are used, never
the lunapi convenience wrappers: several of those construct a new
``lunapi.proj``, which turns logging back on.
"""

import math

import numpy as np

_INT32_MAX = 2**31 - 1
_NA_STRINGS = ("NA", "nan", "NaN", "")


def _is_missing(v):
    if v is None:
        return True
    if isinstance(v, float) and math.isnan(v):
        return True
    if isinstance(v, str) and v in _NA_STRINGS:
        return True
    return False


def _column(values):
    """Return {'type': 'i'|'d'|'s'|'l', 'v': ndarray, 'na': ndarray[bool]}."""
    values = list(values)
    n = len(values)
    na = np.zeros(n, dtype=bool)
    kinds = set()
    for i, v in enumerate(values):
        if _is_missing(v):
            na[i] = True
        elif isinstance(v, (bool, np.bool_, int, np.integer)):
            kinds.add("i")
        elif isinstance(v, (float, np.floating)):
            kinds.add("d")
        else:
            kinds.add("s")

    if kinds and kinds <= {"i"}:
        ints = [0 if na[i] else int(v) for i, v in enumerate(values)]
        if all(-_INT32_MAX <= x <= _INT32_MAX for x in ints):
            return {"type": "i", "v": np.asarray(ints, dtype=np.int32), "na": na}
        kinds = {"d"}

    if kinds and kinds <= {"i", "d"}:
        dbl = [math.nan if na[i] else float(v) for i, v in enumerate(values)]
        return {"type": "d", "v": np.asarray(dbl, dtype=np.float64), "na": na}

    if not kinds:
        return {"type": "l", "v": np.zeros(n, dtype=bool), "na": na}

    strs = ["" if na[i] else str(v) for i, v in enumerate(values)]
    return {"type": "s", "v": np.asarray(strs, dtype=str), "na": na}


def _frame(cols, columns):
    return {"cols": [str(c) for c in cols], "data": [_column(c) for c in columns]}


# ---------------------------------------------------------------------------
# result tables

def strata(store):
    """[[command, strata], ...] for an engine or instance result store."""
    return [[str(c), str(s)] for c, s in store.strata()]


def table(store, cmd, strata):
    r = store.table(cmd, strata)
    return _frame(r[0], r[1])


def dict_strata(ts):
    """[[cmd, strata], ...] for a results dict {cmd: {strata: (cols, cols_data)}}."""
    return [[str(c), str(s)] for c in ts for s in ts[c]]


def dict_table(ts, cmd, strata):
    r = ts[cmd][strata]
    return _frame(r[0], r[1])


def store_tables(store):
    """A snapshot of all tables in an engine or instance result store."""
    return store.tables()


def frame(df):
    """A pandas DataFrame as {'cols', 'data'} (index dropped)."""
    if df is None:
        return None
    return _frame(list(df.columns), [df[c].tolist() for c in df.columns])


# ---------------------------------------------------------------------------
# intervals and time-points

def tp_matrix(pairs):
    """[(start_tp, stop_tp), ...] -> float64 (n, 2) array."""
    a = np.asarray([[float(x[0]), float(x[1])] for x in pairs], dtype=np.float64)
    return a.reshape(-1, 2)


def tp_pairs(m):
    """(n, 2) array of time-points (as floats) -> [(int, int), ...]."""
    m = np.asarray(m, dtype=np.float64).reshape(-1, 2)
    return [(int(round(a)), int(round(b))) for a, b in m]


def sec_pairs(m):
    """(n, 2) array of seconds -> [(float, float), ...]."""
    m = np.asarray(m, dtype=np.float64).reshape(-1, 2)
    return [(float(a), float(b)) for a, b in m]


def e2i(inst, epochs):
    return tp_matrix(inst.e2i([int(e) for e in np.atleast_1d(epochs)]))


def s2i(inst, m):
    return tp_matrix(inst.s2i(sec_pairs(m)))


# ---------------------------------------------------------------------------
# signals

def _mat(m, ncol):
    return np.asarray(m, dtype=np.float64).reshape(-1, ncol)


def data(inst, chs, annots, time):
    cols, m = inst.data(list(chs), list(annots), bool(time))
    cols = [str(c) for c in cols]
    return {"cols": cols, "data": _mat(m, len(cols))}


def slice(inst, m, chs, annots, time):
    cols, x = inst.slice(tp_pairs(m), list(chs), list(annots), bool(time))
    cols = [str(c) for c in cols]
    return {"cols": cols, "data": _mat(x, len(cols))}


def slices(inst, m, chs, annots, time):
    cols, xs = inst.slices(tp_pairs(m), list(chs), list(annots), bool(time))
    cols = [str(c) for c in cols]
    return {"cols": cols, "data": [_mat(x, len(cols)) for x in xs]}


def insert_signal(inst, label, x, sr):
    return inst.insert_signal(str(label), np.asarray(x, dtype=np.float64), int(sr))


def update_signal(inst, label, x):
    return inst.update_signal(str(label), np.asarray(x, dtype=np.float64))


def insert_annot(inst, label, m, durcol2):
    return inst.insert_annot(str(label), sec_pairs(m), bool(durcol2))


# ---------------------------------------------------------------------------
# annotations (sorted and rounded as lunapi's inst.fetch_annots does)

def fetch_annots(inst, anns, interp):
    rows = inst.fetch_annots(list(anns), float(interp))
    rows = sorted(rows, key=lambda r: (r[1], r[2], r[0]))
    return _frame(
        ["Class", "Start", "Stop"],
        [[str(r[0]) for r in rows],
         [round(float(r[1]), 3) for r in rows],
         [round(float(r[2]), 3) for r in rows]])


def fetch_full_annots(inst, anns, add_keys):
    rows = inst.fetch_full_annots(list(anns), bool(add_keys))
    rows = sorted(rows, key=lambda r: (r[4], r[5], r[0], r[1]))
    return _frame(
        ["Class", "Instance", "Channel", "Meta", "Start", "Stop"],
        [[str(r[0]) for r in rows], [str(r[1]) for r in rows],
         [str(r[2]) for r in rows], [str(r[3]) for r in rows],
         [round(float(r[4]), 3) for r in rows],
         [round(float(r[5]), 3) for r in rows]])


# ---------------------------------------------------------------------------
# misc

def sample_list(eng):
    """[[id, edf, [annots...]], ...] (annotation sets as sorted lists)."""
    return [[str(r[0]), str(r[1]), sorted(str(a) for a in r[2])]
            for r in eng.get_sample_list()]


def annots_of(eng, n):
    return sorted(str(a) for a in eng.get_annot(int(n)))


def strs(x):
    return [str(v) for v in x]


def str_set(x):
    return set(str(v) for v in x)
