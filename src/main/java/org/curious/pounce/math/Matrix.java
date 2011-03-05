/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.curious.pounce.math;

/**
 *
 * @author Jarl
 */
public class Matrix {
    public double ul, ll, ur, lr;
    public static final Matrix reg0;

    static{
        reg0 = new Matrix(0, 0, 0, 0);
    }

    public Matrix(double ul, double ll, double ur, double lr){
        this.ul = ul;
        this.ll = ll;
        this.ur = ur;
        this.lr = lr;
    }

    public static Matrix mul(Matrix one, Matrix two){
        Matrix destination = new Matrix(0, 0, 0, 0);
        mul(one, two, destination);
        return destination;
    }

    public static void mul(Matrix one, Matrix two, Matrix destination){
        destination.ul = one.ul*two.ul + one.ur*two.ll;
        destination.ll = one.ll*two.ul + one.lr*two.ll;
        destination.ur = one.ul*two.ur + one.ur*two.lr;
        destination.lr = one.ll*two.ur + one.lr*two.lr;
    }

    public static Vector mul(Matrix one, Vector two){
        Vector destination = new Vector(0, 0);
        mul(one, two, destination);
        return destination;
    }

    public static void mul(Matrix one, Vector two, Vector destination){
        destination.x = one.ul*two.x + one.ur*two.y;
        destination.y = one.ll*two.x + one.lr*two.y;
    }

    public static Matrix rotationMatrix(double theta){
        return new Matrix(Math.cos(theta), Math.sin(theta), -Math.sin(theta), Math.cos(theta));
    }
}
