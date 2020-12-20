
object DynamicProgramming {
def maxSnapsack( maxWeight: Int, wt: Array[Int], values: Array[Int]): Int =
{
  val K = Array.ofDim[Int](wt.size+1,maxWeight+1)

  for(i <- 0 to wt.size)
  {
    for(w <- 0 to maxWeight)
    {
      if( i == 0 || w == 0) K(i)(w)=0
      else if ( wt(i-1) <= w) K(i)(w) = K(i-1)(w).max( K(i-1)(w-wt(i-1)) + values(i-1))
      else K(i)(w) = K(i-1)(w)
    }

   }

  K(wt.size)(maxWeight)
}
}
