def countdown():
    yield 3
    yield 2
    yield 1
    yield "Blast Off!"

g=countdown()
next(g)
