; 4-direction only A* algo for hellfire
; by morduun, with the aid of turtle's tut

; F = 'best-fit' value, is calc'd by combining G + H
; G = movement value.  10 for plains, 20 for brush and forest, and 30 for hills.
;		normally you'd do other stuff for 8-directional movement, but since hellfire
;		only allows 4-directional, this is fine for my application.
; H = guesstimate of how long it takes to get from this square to the end.
;		the "manhattan" estimate is basically an assessment of G from the current position
;		to the target, without taking non-walkable areas into account.

Graphics 320, 240, 32, 2

Include "lists.bb"
SeedRnd MilliSecs()

Type tile
	Field x
	Field y
	Field walkable
	Field parent.tile
	Field f
	Field g
	Field h
	Field myNode.node
End Type

Type waypoint
	Field x
	Field y
	Field myNext.waypoint
End Type


Type coord
	Field x%
	Field y%
End Type

Global start.coord = New coord		
Global goal.coord = New coord		

Global open.list = newList()
Global closed.list = newList()

Global mapsize = 15



Dim map.tile(mapsize-1, mapsize-1)

; initialize the map with random data
; 70% chance of plains
; 10% chance of brush/forest
; 10% chance of hills
; 10% chance of obstacle
For x = 0 To mapsize-1
	For y = 0 To mapsize-1
	
		map.tile(x, y) = newTile()
		map(x, y)\x = x
		map(x, y)\y = y
		
		which = Rnd(1, 10)
		
		If (which <= 7)
				map(x, y)\walkable = True
				map(x, y)\g = 1

		ElseIf which = 8
				map(x, y)\walkable = True
				map(x, y)\g = 2

		ElseIf which = 9
				map(x, y)\walkable = True
				map(x, y)\g = 3
				
		Else
				map(x, y)\walkable = False
		EndIf
		
	Next
Next

; initialize start and goal locations randomly

While (map(start\x, start\y)\walkable = False) Or (randStart = False)
	randStart = True
	start\x = Rand(0, mapsize-1)
	start\y = Rand(0, mapsize-1)
Wend

While (map(goal\x, goal\y)\walkable = False) Or (randGoal = False)
	randGoal = True
	goal\x = Rand(0, mapsize-1)
	goal\y = Rand(0, mapsize-1)
Wend

For x = 0 To mapsize-1
	For y = 0 To mapsize-1
	
		If Not ((x = start\x And y = start\y) Or (x = goal\x And y = goal\y))
			Color 255, 255, 255
		ElseIf (x = start\X And y = start\y)
			Color 0, 128, 255
		Else
			Color 0, 255, 0
		EndIf
		
		Text x * 12, y * 12, map(x, y)\g
		
	Next
Next

Color 0, 128, 255
Rect 220, 10, 8, 8, True
Color 255, 255, 0
Text 230, 8, "=Start"

Color 0, 255, 0
Rect 220, 22, 8, 8, True
Color 255, 255, 0
Text 230, 20, "=End"

Text 220, 40, "0=Obstacle"
Text 220, 52, "1-3=Terrain"
Text 220, 64, " 1=Easy"
Text 220, 76, " 2=Med. "
Text 220, 88, " 3=Hard"

Color 255, 0, 0
Text 5, 200, "Press a key to plot path..."
WaitKey()
Color 0, 0, 0
Rect 5, 200, 320, 40, True
Color 255, 0, 0
begin = MilliSecs()
go.waypoint = getWay(start, goal)
elapsed = MilliSecs() - begin



While go <> Null

	While myNext > MilliSecs(): Delay 10:Wend
	myNext = MilliSecs() + 300
	Text (go\x * 12) , (go\y * 12) , "+"
	go = go\myNext
	count = count + 1

Wend	

Text 5, 200, count + " waypoints generated"
Text 5, 212, elapsed + " millisecs taken"

WaitKey
End


; plug in two coordinate sets, get back the first waypoint in the best path to the AI goal.
Function getWay.waypoint(start.coord, goal.coord)

	; prime the pump
	addList(open, map(start\x, start\y)\myNode)
	
; while the goal isn't on the open list, and while there are still open tiles to check.
	While (Not isList(open, map(goal\x, goal\y)\myNode)) And (countList(open) > 0)	

		bestF.tile = getLowF(open, goal)
		xferList(bestF\myNode, closed)
		evalNeighbors(bestF)
		
	Wend
	
	RVAL.waypoint = genWay(start, goal)
	
	Return RVAL

End Function

; return the tile with the lowest F value
; F being a function of G + H, where
; G is the movement cost of a tile, and
; H is the guessed cost of movement to the goal.
Function getLowF.tile(myList.list, goal.coord)

	Local bestF% = 99999
	
	setPoint(myList, "START")
	
	For i = 1 To countList(myList)
		this.node = getPoint(myList)
		test.tile = this\parent
		
		myG = test\G + 1				; a little tinkering here. -- this =should= be just plain G.
										; doing this makes avoiding bad terrain a slightly higher priority
		myH = getManhattan(test, goal)
		myF = myG + myH
		
		If myF <= bestF
			RVAL.tile = test
			bestF = myF
		EndIf
		setPoint(myList, "NEXT")
	Next
	
	Return RVAL

End Function

; simple distance heuristic.  difference in straight-lines multiplied by 10.
; might modify this to peek at terrain too.
Function getManhattan(this.tile, where.coord)

	mX = (Abs(this\x - where\x) * 1) 
	mY = (Abs(this\y - where\y) * 1) 
	Return mx + my

End Function

; check neighbors for walkability and for possible pathing.
; this version only does four directions, as it's meant for a tile-based
; RPG tac map.  easily modified to work in 8 directions though.
Function evalNeighbors(this.tile)

	Local toggle% = -1
	
	For i = 1 To 4
		toggle = -1 * toggle
		
		If i < 3
			ckX = 0
			ckY = toggle
		Else
			ckX = toggle
			ckY = 0
		EndIf
		
		If (this\x + ckX < 0) Or (this\x + ckX > (mapsize-1)) Or (this\y + ckY < 0) Or (this\y + ckY > (mapsize-1))
		
			; do nothing; this tile is out of range
			
		Else
		
			check.tile = getTile(this\x + ckX, this\Y + ckY)

			If (Not check\walkable) Or (isList(closed, check\myNode))
			
				; do nothing; this tile cannot be used
				
			Else
			
				If (Not isList(open, check\myNode))
					addList(open, check\myNode)
					addParent(check, this)
				Else
					If check\g <= this\parent\g
						this\parent = check
;						check\parent = this
					EndIf
				EndIf
			
			EndIf
			
		EndIf
		
	Next

End Function


Function getTile.tile(x, y)
	If x < 0 Or y < 0 Or x > (mapsize-1) Or y > (mapsize-1)
		Return Null
	Else
		Return map(x, y)
	EndIf
End Function


Function genWay.waypoint(start.coord, goal.coord)

	x = goal\x
	y = goal\y
	
	myTile.tile = getTile(x, y)
	
	If myTile\parent = Null
		Return Null
	EndIf
	
	Repeat
	
		myWay.Waypoint = New Waypoint
		myWay\x = x
		myWay\y = y
		RVAL.waypoint = addWay(myWay, RVAL)
		myTile = myTile\parent
		x = myTile\x
		y = myTile\y
		
		If x = start\x
			If y = start\y
				Exit
			EndIf
		EndIf
		
	Forever
	
	Return RVAL
	
End Function


Function addWay.waypoint(this.waypoint, RVAL.waypoint)

	this\myNext = RVAL
	Return this

End Function


; parent a tile to another tile
Function addParent(kidTile.tile, popTile.tile)
	kidTile\parent = poptile
End Function