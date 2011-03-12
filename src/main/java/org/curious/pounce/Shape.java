/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.curious.pounce;

import java.awt.Graphics;
import java.util.List;
import org.curious.pounce.math.Transformation;
import org.curious.pounce.math.Vector;

/**
 *
 * @author Jarl
 */
public abstract class Shape {
   public double mass;
   public double momentOfIntertia;
   public Vector center;
   public abstract List<Edge> normals(Vector direction);
   public abstract Projection projection(Vector line);
   public abstract Shape transform(Transformation transformation);
   public abstract Shape translate(Vector translation);
   public abstract void render(Graphics graphics);
}
