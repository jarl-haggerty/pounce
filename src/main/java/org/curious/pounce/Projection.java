/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.curious.pounce;

import org.curious.pounce.math.Vector;

/**
 *
 * @author Jarl
 */
public class Projection{
    public final Vector startPoint1, startPoint2, endPoint1, endPoint2;
    public final double start, stop;

    public Projection(Vector startPoint1, Vector startPoint2,
                      Vector endPoint1, Vector endPoint2,
                      double start, double stop){
        this.startPoint1 = startPoint1;
        this.startPoint2 = startPoint2;
        this.endPoint1 = endPoint1;
        this.endPoint2 = endPoint2;
        this.start = start;
        this.stop = stop;
    }
}
