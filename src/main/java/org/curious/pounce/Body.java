/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.curious.pounce;

import java.util.List;
import org.curious.pounce.math.Matrix;
import org.curious.pounce.math.Transformation;
import org.curious.pounce.math.Vector;

/**
 *
 * @author Jarl
 */
public class Body{
    public final Transformation transformation;
    public final List<Shape> shapes;
    public final double mass;
    public final double momentOfInertia;
    public final Vector center;
    public final Vector centerOfMass;
    public final Vector linearMomentum;
    public final double angularMomentum;

    public Body(Transformation transformation, List<Shape> shapes){
        this(transformation, shapes, new Vector(0, 0), 0);
    }

    public Body(Transformation transformation, List<Shape> shapes, Vector linearMomentum, double angularMomentum){
        this.transformation = transformation;
        this.shapes = shapes;

        double mass = 0;
        Vector.fill(Vector.reg0, 0, 0);

        for(Shape shape : shapes){
            mass += shape.mass;
            Vector.fill(Vector.reg1, shape.center);
            Vector.scale(Vector.reg1, shape.mass, Vector.reg1);
            Vector.add(Vector.reg0, Vector.reg1, Vector.reg0);
        }
        this.mass = mass;

        center = new Vector(0, 0);
        for(Shape shape : shapes){
            Vector.add(center, shape.center, center);
        }
        Vector.scale(center, 1/shapes.size(), center);

        if(mass == Double.POSITIVE_INFINITY){
            centerOfMass = center;
        }else{
            centerOfMass = new Vector(0, 0);
            for(Shape shape : shapes){
                Vector.fill(Vector.reg0, shape.center);
                Vector.scale(Vector.reg0, shape.mass, Vector.reg0);
                Vector.add(centerOfMass, Vector.reg0, centerOfMass);
            }
            Vector.scale(centerOfMass, 1/shapes.size(), centerOfMass);
        }

        double momentOfInertia = 0;
        for(Shape shape : shapes){
            Vector.sub(shape.center, centerOfMass, Vector.reg0);
            momentOfInertia += shape.momentOfIntertia + shape.mass*Vector.lengthSquared(Vector.reg0);
        }
        this.momentOfInertia = momentOfInertia;

        this.linearMomentum = linearMomentum;
        this.angularMomentum = angularMomentum;
    }

    public Body(Body body, double delta, Vector force, double torque){
        shapes = body.shapes;
        mass = body.mass;
        center = body.center;
        centerOfMass = body.centerOfMass;
        momentOfInertia = body.momentOfInertia;
        linearMomentum = new Vector(force);
        Vector.scale(linearMomentum, delta, linearMomentum);
        Vector.add(linearMomentum, body.linearMomentum, linearMomentum);
        angularMomentum = torque*delta + body.angularMomentum;

        Matrix rotation = Matrix.rotationMatrix(angularMomentum*delta/momentOfInertia);
        Matrix.mul(body.transformation.rotation, rotation, rotation);

        Vector translation = new Vector(linearMomentum);
        Vector.scale(translation, delta/mass, translation);
        Vector.add(translation, body.transformation.translation, translation);

        Vector.sub(body.transformation.translation, body.centerOfMass, Vector.reg0);
        Matrix.mul(body.transformation.rotation, Vector.reg0, Vector.reg1);
        Vector.sub(Vector.reg1, Vector.reg0, Vector.reg0);
        Vector.add(translation, Vector.reg0, translation);

        transformation = new Transformation(rotation, translation);
    }
}
