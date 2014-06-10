// this is a short script that counts the number of subtractions that exist between two numbers both between one and one thousand that can be performed without a subtraction
public class noBorrowSubBrute{

public static void main(String args[])
    {
        String betweenonethousand = "";
        int noborrows = 0;
        for(int i = 0; i <10; i++)
        {
            betweenonethousand = "" + i;
            for (int j = 0; j<10; j++)
            {
                betweenonethousand = betweenonethousand + j;
                for (int k = 0; k<10; k++)
                {
                    betweenonethousand = betweenonethousand + k;
                    noborrows = noborrows + countNoBorrows(betweenonethousand);
                    betweenonethousand = "" + i + "" + k;
                }
                betweenonethousand = "" + i;
            }
            betweenonethousand = "";
        }
        System.out.println("" + noborrows);
    }
    public static int countNoBorrows(String threedigit)
    {
        int ip = 0;
        int jp = 0;
        int kp = 0;
        ip = Character.getNumericValue(threedigit.charAt(0));
        jp = Character.getNumericValue(threedigit.charAt(1));
        kp = Character.getNumericValue(threedigit.charAt(2));
        int noborrowstemp = (ip+1)*(jp+1)*(kp+1);
        return noborrowstemp;
    }


}
