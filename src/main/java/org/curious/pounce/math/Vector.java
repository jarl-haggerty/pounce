/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.curious.pounce.math;

import clojure.lang.AFn;
import clojure.lang.ISeq;
import clojure.lang.RT;

/**
 *
 * @author Jarl
 */
public class Vector {
    public double x, y;
    public static final Vector reg0, reg1, reg2, reg3, reg4, reg5, reg6, reg7, reg8, reg9, zero;

    static{
        reg0 = new Vector(0, 0);
        reg1 = new Vector(0, 0);
        reg2 = new Vector(0, 0);
        reg3 = new Vector(0, 0);
        reg4 = new Vector(0, 0);
        reg5 = new Vector(0, 0);
        reg6 = new Vector(0, 0);
        reg7 = new Vector(0, 0);
        reg8 = new Vector(0, 0);
        reg9 = new Vector(0, 0);
        zero = new Vector(0, 0);

        RT.var("org.curious.pounce.math.matrix", "vector", new AFn(){
            @Override
            public Object invoke(Object source){
                if(source instanceof Vector){
                    return source;
                }else{
                    ISeq seq = (ISeq)source;
                    return new Vector(((Number)seq.first()).doubleValue(), ((Number)seq.next().first()).doubleValue());
                }
            }
            @Override
            public Object invoke(Object x, Object y){
                return new Vector(((Number)x).doubleValue(), ((Number)y).doubleValue());
            }
        });
        RT.var("org.curious.pounce.math.matrix", "x", new AFn(){
            @Override
            public Object invoke(Object subject){
                return ((Vector)subject).x;
            }
        });
        RT.var("org.curious.pounce.math.matrix", "y", new AFn(){
            @Override
            public Object invoke(Object subject){
                return ((Vector)subject).y;
            }
        });
    }

    public Vector(Vector source){
        this.x = source.x;
        this.y = source.y;
    }

    public Vector(double x, double y){
        this.x = x;
        this.y = y;
    }

    public static Vector add(Vector one, Vector two){
        Vector destination = new Vector(0, 0);
        add(one, two, destination);
        return destination;
    }

    public static void add(Vector one, Vector two, Vector destination){
        destination.x = one.x + two.x;
        destination.y = one.y + two.y;
    }

    public static Vector sub(Vector one, Vector two){
        Vector destination = new Vector(0, 0);
        sub(one, two, destination);
        return destination;
    }

    public static void sub(Vector one, Vector two, Vector destination){
        destination.x = one.x - two.x;
        destination.y = one.y - two.y;
    }

    public static double dot(Vector one, Vector two){
        return one.x*two.x + one.y*two.y;
    }

    public static double cross(Vector one, Vector two){
        return one.x*two.y - one.y*two.x;
    }

    public static double lengthSquared(Vector input){
        return dot(input, input);
    }

    public static double length(Vector input){
        return Math.sqrt(lengthSquared(input));
    }

    public static Vector normalize(Vector input){
        Vector output = new Vector(0, 0);
        normalize(input, output);
        return output;
    }

    public static void normalize(Vector input, Vector output){
        double len = length(input);
        output.x = input.x/len;
        output.y = input.y/len;
    }
    
    public static Vector scale(Vector source, double factor){
        Vector destination = new Vector(0, 0);
        scale(source, factor, destination);
        return destination;
    }

    public static void scale(Vector source, double factor, Vector destination){
        destination.x = factor*source.x;
        destination.y = factor*source.y;
    }

    public static void fill(Vector vec, double x, double y){
        vec.x = x;
        vec.y = y;
    }

    public static void fill(Vector vec, Vector source){
        vec.x = source.x;
        vec.y = source.y;
    }

    @Override
    public String toString(){
        return "(" + x + ", " + y + ")";
    }
}
