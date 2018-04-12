defmodule Elixir_Intro do

  # to debug:
  # :debugger.start()
  # :int.ni(Elixir_Intro)
  # :int.break(Elixir_Intro, {line number})


  def fib(1) do 1 end
  def fib(2) do 1 end
  def fib(n) do
    fib(n - 1) + fib(n - 2)
  end

  #-------------------------------------------

  def area(shape, shape_info) do
    case shape do
      :rectangle -> elem(shape_info, 0) * elem(shape_info, 1)
      :square -> shape_info * shape_info
      :circle -> :math.pi * shape_info * shape_info
      :triangle -> 0.5 * elem(shape_info, 0) * elem(shape_info, 1)
    end
  end

  #-------------------------------------------

  def sqrList(nums) do
    for n <- nums do n*n end
  end

  #-------------------------------------------

  def calcTotals(inventory) do
    map(&Elixir_Intro.getItemTotal/1, inventory)
  end

  def getItemTotal(item) do
    {elem(item, 0), elem(item, 1) * elem(item, 2)}
  end

  #-------------------------------------------

  def map(function, vals) do
    for v <- vals do function.(v) end
  end

  #Elixir_Intro.map(&Elixir_Intro.testMap/1, list)
  def testMap(x) do
    x + 1
  end

  #-------------------------------------------

  def quickSortServer() do
    receive do
      {list, pid} ->
        send(pid,{quickSort(list),self()})
    end
    quickSortServer()
  end

  def quickSort([]) do [] end
  def quickSort(list) do
    lengthOfList = length(list) - 1
    pivotIndex = Enum.random(0..lengthOfList)
    pivot = Enum.at(list, pivotIndex)
    rest = List.delete_at(list, pivotIndex)
    smaller = for n <- rest, n < pivot do n end
    larger = for n <- rest, n >= pivot do n end
    quickSort(smaller) ++ [pivot] ++ quickSort(larger)
  end

  #-------------------------------------------

  def callServer(pid,list) do
    send(pid, {list, self()})
	  listen()
  end

  def listen do
    receive do
	    {sorted, pid} -> sorted
	  end
  end

end #defmodule
