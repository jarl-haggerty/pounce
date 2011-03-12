/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.curious.pounce.math;

/**
 *
 * @author Jarl
 */
public class Transformation {
    public final Matrix rotation;
    public final Vector translation;
    public static final Transformation identity;

    static{
        identity = new Transformation(Matrix.identity, Vector.zero);
    }

    public Transformation(Matrix rotation, Vector translation){
        this.rotation = rotation;
        this.translation = translation;
    }

    public static Vector tranform(Transformation transformation, Vector source){
        Vector destination = new Vector(0, 0);
        transform(transformation, source, destination);
        return destination;
    }

    public static void transform(Transformation transformation, Vector source, Vector destination){
        Matrix.mul(transformation.rotation, source, destination);
        Vector.add(transformation.translation, destination, destination);
    }
}
