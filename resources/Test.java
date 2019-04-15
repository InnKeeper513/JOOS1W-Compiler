// PARSER_WEEDER
/**
 * This method is supposed to test whether access to the resulting
 * objects of method calls are parsed correctly.
 **/

package ca.uwaterloo;

import A.B.C;
import A.*;
import A;

public final class Test extends A implements A.D, A.E {

    public static int i = 1 + 3;

    public Test(int j, int k){
	    for ( int l = 0; l < 1; l = l + 1) {
	        i = l;
        }

        while (true) {
            i = 1;
            i = i - 1;
        }

        i = j;
    }

    public Test inc(){
	return new Test(i+1);
    }

    public static int test(){
        return new Test(120).inc().inc().inc().i;
    }
    public static int test(int j, int k){
        return new Test(120).inc().inc().inc().i;
    }

}
