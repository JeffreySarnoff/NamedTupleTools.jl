nt = (a = 1, b = "two", c = '3', d = 4//1)

@test keep(nt, :b) == (b = "two",)
@test keep(nt, (:b, :c)) == (b = "two", c = '3')
@test keep(nt, (:c, :b)) == (c = '3', b = "two")

@test keep(nt, 2) == (b = "two",)
@test keep(nt, (2, 3)) == (b = "two", c = '3')
@test keep(nt, (3, 2)) == (c = '3', b = "two")

@test omit(nt, :b) == (a = 1, c = '3', d = 4//1)
@test omit(nt, (:b, :c)) == (a = 1, d = 4//1)
@test omit(nt, (:c, :b)) == (a = 1, d = 4//1)

@test omit(nt, 2) == (a = 1, c = '3', d = 4//1)
@test omit(nt, (2, 3)) == (a = 1, d = 4//1)
@test omit(nt, (3, 2)) == (a = 1, d = 4//1)

