; because I screw this up for hours before I get it right each time I do it,
; this is a lib to implement manual linked list functions
; for any generic type with a myNext.type / myPrev.type structure.
;
; - lists are completely generic and need no editing ever.
; - nodes are almost completely generic; they only need their parent field
;	modified to reflect the proper type they refer to.
; - types need only a reference to a node to participate in lists.

; =======================================================================
; the list type.  references its first node, its last node, a pointer to the 'current'
; node in the list, and the total count of nodes in the list.
Type list
	Field myFirst.node
	Field myLast.node
	Field myPoint.node
	Field count%
End Type

; =======================================================================
; the node type.  references its list, its two neighbors in the list, and the type
; instance that it relates to.
Type node
	Field myList.list
	Field myNext.node
	Field myPrev.node
	Field parent.tile		; only this needs to change for each new version
End Type

; =======================================================================
; creates a new type instance
Function newTile.Tile()

	RVAL.tile = New Tile
	RVAL\myNode.node = New node
	RVAL\myNode\parent = RVAL
	Return RVAL

End Function

; =======================================================================
; creates a new type list
Function newList.list()

	RVAL.list = New list
	Return RVAL

End Function

; =======================================================================
; adds a node to the bottom of a list
Function addList(myList.list, myNode.node)

	If myList = Null Then Return False
	If myNode = Null Then Return False
	
	If myList\myFirst = Null
	
		myList\myFirst = myNode
		myList\myLast = myNode
		myList\myPoint = myNode
		myList\count = myList\count + 1
		myNode\myList = myList
		Return True
	
	Else
	
		myNode\myPrev = myList\myLast
		myNode\myList = myList

		myList\myLast\myNext = myNode
		myList\myLast = myNode
		myList\count = myList\count + 1
		Return True
		
	EndIf
	
	Return False

End Function

; =======================================================================
; inserts a node before the current pointer
Function insList(myList.list, myNode.node)

	If myList = Null Then Return False
	If myNode = Null Then Return False
	If myList\myPoint = Null 					; no type instances in this list
		addList(myList, myNode)					; add as per normal
	Else
		beforePoint.node = myList\myPoint\myPrev
		myList\myPoint\myPrev = myNode
		
		myNode\myNext = myList\myPoint
		myNode\myPrev = beforePoint
		
		If myNode\myPrev = Null
			myList\myFirst = myNode
		Else
			myNode\myPrev\myNext = myNode
		EndIf
		
		myList\myPoint = myNode
		myList\count = myList\count + 1
	EndIf

End Function

; =======================================================================
; removes a node from a list
Function remList(myNode.node)

	If myNode = Null Then Return False

	Local myList.list = myNode\myList
	If myList = Null Then Return False
	
; heal the list pointers
	If myNode = myList\myFirst Then myList\myFirst = myNode\myNext
	If myNode = myList\myLast Then myList\myLast = myNode\myPrev
	
	If myList\myPoint = myNode
		If myNode\myPrev <> Null
			myList\myPoint = myNode\myPrev
		ElseIf myNode\myNext <> Null
			myList\myPoint = myNode\myNext
		Else
			myList\myPoint = Null
		EndIf
	EndIf
	
; heal the previous node
	If myNode\myPrev <> Null Then myNode\myPrev\myNext = myNode\myNext
	
; heal the following node
	If myNode\myNext <> Null Then myNode\myNext\myPrev = myNode\myPrev
	
; update the count of instances in the list
	myList\count = myList\count - 1	

; remove all list references from this node
	myNode\myPrev = Null
	myNode\myNext = Null
	myNode\myList = Null
	
	Return True

End Function

; =======================================================================
; clears all nodes from a list.
Function clearList(myList.list)

	If myList = Null Then Return False
	If myList\myFirst = Null Then Return False 
	
	test.node = myList\myFirst
	
	While test <> Null
		If test\myPrev <> Null Then test\myPrev\myNext = Null
		test\myPrev = Null
		test\myList = Null
		test = test\myNext
	Wend
	
	myList\myFirst = Null
	myList\myLast = Null
	myList\myPoint = Null
	myList\count = 0
	
	Return True

End Function

; =======================================================================
; transfers a node from its current list to a new list.
Function xferList(myNode.node, toList.list)

	remList(myNode)
	addList(toList, myNode)

End Function

; =======================================================================
; returns true if a node is a member of a list.
Function isList(myList.list, myNode.node)

	If mynode\myList = myList
		Return True
	Else
		Return False
	EndIf

End Function

; =======================================================================
; returns the number of nodes in a list
Function countList(myList.list)
	Return myList\count
End Function

; =======================================================================
; sets a list pointer using a parameter.
; START sets the pointer to the top of the list
; END sets the pointer to the end of the list
; NEXT advances the pointer to the next node in the list
; PREV backs up the pointer to the previous node in the list
; a number will move the pointer to that ordinal in the list
Function setPoint(myList.list, param$ = "START")

	Select param$
		Case "START"
			myList\myPoint = myList\myFirst
			Return True
			
		Case "END"
			myList\myPoint = myList\myLast
			Return True
			
		Case "NEXT"
			If myList\myPoint\myNext <> Null
				myList\myPoint = myList\myPoint\myNext
				Return True
			Else
				Return False
			EndIf
			
		Case "PREV"
			If myList\myPoint\myPrev <> Null
				myList\myPoint = myList\myPoint\myPrev
				Return True
			Else
				Return False
			EndIf
			
		Default	; has to be a numerical value
		
			test.node = myList\myFirst
			iter% = param$
			
			For i = 1 To (iter - 1)
			
				If test = Null Then Return False
				test = test\myNext
			
			Next
			
			myList\myPoint = test
			Return True
			
	End Select

End Function

; =======================================================================
; checks to see if a type instance exists
; NEXT checks if there is a node after the current pointer.
; PREV checks if there is a node before the current pointer.
; a number will check to see if that ordinal exists.
Function checkPoint(myList.list, param$)

	Select param$
		
		Case "NEXT"
			If myList\myPoint\myNext <> Null
				Return True
			Else
				Return False
			EndIf
			
		Case "PREV"
			If myList\myPoint\myPrev <> Null
				Return True
			Else
				Return False
			EndIf
			
		Default	; has to be a numerical value
		
			test.node = myList\myFirst
			
			For i = 1 To param
			
				If test = Null Then Return False
				test = test\myNext

			Next
			
			Return True
			
	End Select
	
End Function

; =======================================================================
; returns the currently pointed-to node.
Function getPoint.node(myList.list)

	Return myList\myPoint

End Function