package com.mateos.playingAround

object worksheet {


	def nth[T](n: Int, list: List[T]): T = {
		if (list isEmpty) throw new IndexOutOfBoundsException
		else if (n==0) list.head
		else nth(n-1, list.tail)
	
	}                                         //> nth: [T](n: Int, list: List[T])T
}