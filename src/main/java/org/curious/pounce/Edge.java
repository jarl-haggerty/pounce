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
public class Edge{
    Vector from, to, normal;

    public Edge(Vector from, Vector to){
        this.from = from;
        this.to = to;
        normal = Vector.sub(to, from);
        Vector.normalize(normal, normal);
        double temp = normal.x;
        normal.x = -normal.y;
        normal.y = temp;
    }
    
    public Edge(Vector from, Vector to, Vector normal){
        this.from = from;
        this.to = to;
        this.normal = normal;
    }
}
