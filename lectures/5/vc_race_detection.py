class DataRaceError(Exception):
    """Base class for all data race related exceptions."""

    pass


class WriteReadDataRace(DataRaceError):
    def __init__(self, reason=""):
        self.message = reason
        super().__init__(self.message)


class ReadWriteDataRace(DataRaceError):
    def __init__(self, reason=""):
        self.message = reason
        super().__init__(self.message)


class WriteWriteDataRace(DataRaceError):
    def __init__(self, reason=""):
        self.message = reason
        super().__init__(self.message)


class RaceDetector:
    def __init__(
        self, num_threads: int, num_locks: int, num_locations: int, debug=False
    ):
        self.debug = debug
        self.num_threads: int = num_threads
        self.num_locks: int = num_locks
        self.num_locations: int = num_locations

        self.C = [
            [1 if i == j else 0 for i in range(self.num_threads)]
            for j in range(self.num_threads)
        ]
        self.L = [[0] * self.num_threads for _ in range(self.num_locks)]
        self.R = [[0] * self.num_threads for _ in range(self.num_locations)]
        self.W = [[0] * self.num_threads for _ in range(self.num_locations)]

    def acq(self, t: int, m: int):
        self.C[t] = join_vc(self.C[t], self.L[m])
        if self.debug:
            print(f"> acc(t={t}, m={m})")
            print(self)

    def rel(self, t: int, m: int):
        self.L[m] = self.C[t].copy()
        self.C[t][t] += 1
        if self.debug:
            print(f"> rel(t={t}, m={m})")
            print(self)

    def rd(self, t: int, x: int):
        if not is_leq(self.W[x], self.C[t]):
            raise WriteReadDataRace(f"W{x} ≤ C{t}")
        self.R[x][t] = self.C[t][t]
        if self.debug:
            print(f"> rd(t={t}, x={x})")
            print(self)

    def wr(self, t: int, x: int):
        if not is_leq(self.W[x], self.C[t]):
            raise WriteWriteDataRace(f"W{x} ≤ C{t}")
        if not is_leq(self.R[x], self.C[t]):
            raise ReadWriteDataRace(f"R{x} ≤ C{t}")
        self.W[x][t] = self.C[t][t]
        if self.debug:
            print(f"> wr(t={t}, x={x})")
            print(self)

    def __str__(self):
        result = "RaceDetector State:\n"
        magic = max(9, 3 * self.num_threads + 2)

        result += f"{'Clock (C)':<{magic}} | {'Locks (L)':<{magic}} | {'Reads (R)':<{magic}} | {'Writes (W)':<{magic}}\n"

        max_rows = max(len(self.C), len(self.L), len(self.R), len(self.W))
        for i in range(max_rows):
            row_C = (
                f"C{i}  " + "  ".join(str(x) for x in self.C[i])
                if i < len(self.C)
                else ""
            )
            row_L = (
                f"L{i}  " + "  ".join(str(x) for x in self.L[i])
                if i < len(self.L)
                else ""
            )
            row_R = (
                f"R{i}  " + "  ".join(str(x) for x in self.R[i])
                if i < len(self.R)
                else ""
            )
            row_W = (
                f"W{i}  " + "  ".join(str(x) for x in self.W[i])
                if i < len(self.W)
                else ""
            )

            result += f"{row_C:<{magic}} | {row_L:<{magic}} | {row_R:<{magic}} | {row_W:<{magic}}\n"

        return result


def join_vc(c1: list, c2: list) -> list:
    assert len(c1) == len(c2)
    result = [0] * len(c1)
    for i in range(len(c1)):
        result[i] = max(c1[i], c2[i])
    return result


def is_leq(c1: list, c2: list) -> bool:
    assert len(c1) == len(c2)
    return all(c1[i] <= c2[i] for i in range(len(c1)))


def example():
    rd = RaceDetector(num_threads=2, num_locks=1, num_locations=3, debug=True)
    print("=== Initial state ===")
    print(rd)

    rd.acq(t=0, m=0)
    rd.rd(t=0, x=0)
    rd.wr(t=0, x=0)
    rd.rel(t=0, m=0)

    rd.acq(t=1, m=0)
    rd.rd(t=1, x=0)
    rd.wr(t=1, x=0)
    rd.rel(t=1, m=0)

    rd.acq(t=1, m=0)
    rd.rd(t=1, x=1)
    rd.wr(t=1, x=1)
    rd.rel(t=1, m=0)

    rd.rd(t=1, x=2)
    rd.wr(t=1, x=2)

    rd.acq(t=0, m=0)
    rd.rd(t=0, x=1)
    rd.wr(t=0, x=1)
    rd.rel(t=0, m=0)

    rd.rd(t=0, x=2)
    rd.wr(t=0, x=2)

    print("=== Final state ===")
    print(rd)


if __name__ == "__main__":
    example()
