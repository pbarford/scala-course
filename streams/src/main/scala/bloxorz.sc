val r = Vector(Vector('S', 'T'), Vector('o', 'o'), Vector('o', 'o'))

r(0)(1)

val cPos = r.map(v => v.indexOf('o'))
val cPosIndex = cPos.indexWhere(_ >= 0)
val p = Vector(cPosIndex, cPos(cPosIndex))