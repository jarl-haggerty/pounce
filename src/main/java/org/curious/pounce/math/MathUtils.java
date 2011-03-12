/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.curious.pounce.math;

/**
 *
 * @author Jarl
 */
public class MathUtils {
    public static final double eps;
    public final Matrix mat0;
    public final Vector vec0, vec1, vec2, vec3, vec4, vec5, vec6, vec7, vec8, vec9;

    public MathUtils(){
        mat0 = new Matrix(0, 0, 0, 0);
        vec0 = new Vector(0, 0);
        vec1 = new Vector(0, 0);
        vec2 = new Vector(0, 0);
        vec3 = new Vector(0, 0);
        vec4 = new Vector(0, 0);
        vec5 = new Vector(0, 0);
        vec6 = new Vector(0, 0);
        vec7 = new Vector(0, 0);
        vec8 = new Vector(0, 0);
        vec9 = new Vector(0, 0);
    }

    static{
        eps = 1e-10;
    }

    public static boolean epsEquals(double x, double y){
        return x == y || x-y < eps && y-x < eps;
    }

    public static boolean epsLessThan(double x, double y){
        return x+eps < y;
    }

    public static boolean epsLessThanOrEqual(double x, double y){
        return x < y+eps;
    }

    public static boolean epsGreaterThan(double x, double y){
        return x+eps > y;
    }

    public static boolean epsGreaterThanOrEqual(double x, double y){
        return x > y+eps;
    }
}
