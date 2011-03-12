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
public class Side {
    public double min, max;
    public Vector minPoint, maxPoint;

    Side(double min, Vector minPoint, double max, Vector maxPoint) {
        this.min = min;
        this.minPoint = minPoint;
        this.max = max;
        this.maxPoint = maxPoint;
    }
}
