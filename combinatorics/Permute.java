public class Permute{
public static void main(String [ ] args)
{
    /* input from the command line */
permute(args[0]);
}

     /* method to preprocess the string and call the permutation recursion*/
public static void permute(String in)
{
    /*  call length so the string in doesnt have to be carried and recalculated*/
    int length = in.length();
    /*  place string in array so multiple accesses are not slow*/
    char [] inarr = in.toCharArray();
    /* output array for ease, though concatenation work as well*/
    char [] outarr = new char[length];
    /* boolean for visted state to minimize size of array*/
    boolean [] visited = new boolean[length];
    /* calls permutation with input*/
callpermute(inarr, outarr, visited, length, 0);
}

public static void callpermute(char [] inarr, char [] outarr, boolean [] visited, int length, int level)
{
    /* test to see if the permutation has gone all the way down. Prints if true.*/
if(level == length)
{
    System.out.println(outarr);
}
else
{
    /*  loop to iterate through each of the letters input*/
for(int i = 0; i < length; i++ )
{
    /*  test to see if that part of the string has been visted, if so continues to \\the next, so recursive calls don't overwrite themselves*/
    if(! visited[i])
{
    /*  current depth in permutation is set to the location in the loop that produces the first false visted value*/
    outarr[level] = inarr[i];
    /* sets the just input value to true*/
    visited[i] = true;
    /*  recursive call to go one level deeper, retaining the true values visited before*/
    callpermute(inarr, outarr, visited, length, level +1);
    /*  reverts visted at the location in the loop to false to allow reordering*/1
    visited[i] = false;
}
}
}
}
}