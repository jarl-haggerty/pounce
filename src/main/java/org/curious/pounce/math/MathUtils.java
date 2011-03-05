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
