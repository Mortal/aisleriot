def key(game: tuple[int, int, int, int]) -> tuple[int, tuple[int,int,int,int]]:
    return (max(abs((x+6)%13-6) for x in game), game)


def rep(game: tuple[int,int,int,int]) -> tuple[int,int,int,int]:
    games = [(a,b,c,d) for k in range(1,13) for a,b,c,d in [sorted(1 + (k * x - 1) % 13 for x in game)]]
    a,b,c,d = min(games, key=key)
    return a,b,c,d


def prn(game: tuple[int,int,int,int]) -> str:
    return "".join("_A23456789TJQK"[x] for x in game)


def main() -> None:
    games = ((a,b,c,d) for a in range(1,13) for b in range(a,13) for c in range(b,13) for d in range(c,13))
    byrep: dict[tuple[int,int,int,int],list[tuple[int,int,int,int]]] = {}
    for g in games:
        r = rep(g)
        assert r == rep(r)
        byrep.setdefault(r, []).append(g)
    for r in byrep:
        print(len(byrep[r]), *(prn(g) for g in sorted(byrep[r], key=key)))


if __name__ == "__main__":
    main()
