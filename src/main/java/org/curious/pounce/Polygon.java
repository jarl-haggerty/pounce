/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.curious.pounce;

import java.awt.Color;
import java.awt.Graphics;
import java.util.Iterator;
import java.util.LinkedList;
import java.util.List;
import org.curious.pounce.math.MathUtils;
import org.curious.pounce.math.Transformation;
import org.curious.pounce.math.Vector;

/**
 *
 * @author Jarl
 */
public class Polygon extends Shape {
    private final List<Edge> edges;
    private final List<Vector> points;

    public Polygon(double mass, List<Vector> points){
        this.mass = mass;
        this.points = points;
        edges = new LinkedList<Edge>();
        Iterator<Vector> iterator = points.iterator();
        Vector temp;
        while(iterator.hasNext()){
            temp = iterator.next();
            if(iterator.hasNext()){
                edges.add(new Edge(temp, iterator.next()));
            }else{
                edges.add(new Edge(temp, points.get(0)));
            }
        }
    }

    public Polygon(double mass, List<Vector> points, List<Edge> edges){
        this.mass = mass;
        this.points = points;
        this.edges = edges;
    }

    public List<Edge> normals(Vector direction) {
        List<Edge> result = new LinkedList<Edge>();
        for(Edge edge : edges){
            if(Vector.dot(direction, edge.normal) >= 0){
                result.add(edge);
            }
        }
        return result;
    }

    public Projection projection(Vector line) {
        Vector startPoint1 = null, startPoint2 = null;
        Vector stopPoint1 = null, stopPoint2 = null;
        double start = Double.POSITIVE_INFINITY, stop = Double.NEGATIVE_INFINITY, temp;

        for(Edge edge : edges){
            temp = Vector.dot(edge.from, line);
            if(MathUtils.epsLessThanOrEqual(temp, start)){
                if(MathUtils.epsEquals(temp, start)){
                    startPoint2 = edge.from;
                }else{
                    startPoint1 = edge.from;
                    startPoint2 = null;
                }
                start = temp;
            }
            if(MathUtils.epsLessThanOrEqual(start, temp)){
                if(MathUtils.epsEquals(start, temp)){
                    stopPoint2 = edge.from;
                }else{
                    stopPoint1 = edge.from;
                    stopPoint2 = null;
                }
                start = temp;
            }
        }

        return new Projection(startPoint1, startPoint2, stopPoint1, stopPoint2, start, stop);
    }

    public Shape transform(Transformation transformation) {
        List<Vector> newPoints = new LinkedList<Vector>();
        for(Vector point : points){
            newPoints.add(Transformation.tranform(transformation, point));
        }
        return new Polygon(mass, newPoints);
    }

    public void render(Graphics graphics) {
        graphics.setColor(Color.white);
        for(Edge edge : edges){
            graphics.drawLine((int)edge.from.x, (int)(graphics.getClipBounds().height-edge.from.y),
                              (int)edge.to.x, (int)(graphics.getClipBounds().height-edge.to.y));
        }
    }

    @Override
    public Shape translate(Vector translation) {
        List<Vector> points = new LinkedList<Vector>();
        for(Vector point : this.points){
            points.add(Vector.add(translation, point));
        }

        List<Edge> edges = new LinkedList<Edge>();
        Iterator<Vector> pointIterator = points.iterator();
        Iterator<Edge> edgeIterator = this.edges.iterator();
        Vector temp;
        while(pointIterator.hasNext()){
            temp = pointIterator.next();
            if(pointIterator.hasNext()){
                edges.add(new Edge(temp, pointIterator.next(), edgeIterator.next().normal));
            }else{
                edges.add(new Edge(temp, points.get(0), edgeIterator.next().normal));
            }
        }

        return new Polygon(mass, points, edges);
    }
}
