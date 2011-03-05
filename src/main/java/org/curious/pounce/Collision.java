/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.curious.pounce;

import clojure.lang.IPersistentCollection;
import clojure.lang.ISeq;
import clojure.lang.ITransientCollection;
import clojure.lang.PersistentHashSet;
import java.util.List;
import org.curious.pounce.math.MathUtils;
import org.curious.pounce.math.Vector;

/**
 *
 * @author Jarl
 */
public class Collision {
    double depth;
    Vector point;

    public Collision(double depth, Vector point){
        this.depth = depth;
        this.point = point;
    }

    public static IPersistentCollection detectCollisions(IPersistentCollection bodies){
        ISeq bodySeq1 = bodies.seq(), bodySeq2;
        Body body1, body2;
        ITransientCollection collisions = PersistentHashSet.EMPTY.asTransient();
        while(bodySeq1.count() != 0){
            body1 = (Body)bodySeq1.first();
            bodySeq2 = bodySeq1.next();
            while(bodySeq2.count() != 0){
                body2 = (Body)bodySeq2.first();
                for(Shape shape1 : body1.shapes){
                    for(Shape shape2 : body2.shapes){
                        Collision[] collision = detectShapeCollision(shape1, shape2);
                        if(collision != null){
                            if(collision[0] != null){
                                collisions.conj(collision[0]);
                            }
                            if(collision[1] != null){
                                collisions.conj(collision[1]);
                            }
                        }
                    }
                }
            }
        }
        return collisions.persistent();
    }

    public static Collision[] detectShapeCollision(Shape one, Shape two){
        Vector.sub(two.center, one.center, Vector.reg0);
        Vector.normalize(Vector.reg0, Vector.reg0);
        Collision[] oneTwo = detectShapeCollision(one, two, Vector.reg0);
        Vector.scale(Vector.reg0, -1, Vector.reg0);
        Collision[] twoOne = detectShapeCollision(one, two, Vector.reg0);

        if(oneTwo != null){
            if(twoOne != null){
                return MathUtils.epsLessThan(oneTwo[0].depth, twoOne[0].depth) ? twoOne : oneTwo;
            }else{
                return oneTwo;
            }
        }
        if(twoOne != null){
            if(oneTwo != null){
                return MathUtils.epsLessThan(twoOne[0].depth, oneTwo[0].depth) ? oneTwo : twoOne;
            }else{
                return twoOne;
            }
        }
        return null;
    }

    public static Collision[] detectShapeCollision(Shape one, Shape two, Vector direction){
        List<Edge> edges = one.normals(direction);
        Collision[] collisions = new Collision[]{new Collision(Double.POSITIVE_INFINITY, Vector.reg8),
                                                 new Collision(Double.POSITIVE_INFINITY, Vector.reg9)};
        Side side1, side2;
        Vector line = Vector.reg1, normalLine = Vector.reg2;

        for(Edge edge : edges){
            Vector.scale(edge.from, -1, Vector.reg1);
            Projection projection = two.translate(Vector.reg1).projection(direction);
            if(MathUtils.epsLessThanOrEqual(projection.start, 0)){
                double depth = -projection.start;
                if(MathUtils.epsLessThan(depth, collisions[0].depth)){
                    Vector.sub(edge.to, edge.from, line);
                    Vector.normalize(line, normalLine);
                    side1 = new Side(0, edge.from, Vector.length(line), edge.to);
                    if(projection.startPoint2 == null){
                        double location = Vector.dot(normalLine, projection.startPoint1);
                        if(location > side1.min && location < side1.max){
                            collisions[0].depth = depth;
                            Vector.scale(normalLine, location, collisions[0].point);
                        }
                    }else{
                        double locationOne = Vector.dot(normalLine, projection.startPoint1);
                        double locationTwo = Vector.dot(normalLine, projection.startPoint2);
                        double temp;
                        if(locationOne > locationTwo){
                            temp = locationOne;
                            locationOne = locationTwo;
                            locationTwo = temp;
                        }
                        Vector.scale(normalLine, locationOne, Vector.reg3);
                        Vector.scale(normalLine, locationTwo, Vector.reg4);
                        side2 = new Side(locationOne, Vector.reg3,
                                         locationTwo, Vector.reg4);

                        if(MathUtils.epsLessThan(side1.min, side2.min) && MathUtils.epsLessThan(side2.min, side1.min) ||
                           MathUtils.epsEquals(side1.min, side2.min) && MathUtils.epsLessThan(side2.min, side1.min)   ||
                           MathUtils.epsLessThan(side1.min, side2.min) && MathUtils.epsEquals(side2.min, side1.min)){

                        }else if(MathUtils.epsEquals(edge.normal.x, 0) && MathUtils.epsGreaterThan(edge.normal.y, 0) ||
                                 MathUtils.epsGreaterThan(edge.normal.x, 0)){
                            if(side1.min == side2.max){

                            }else if(side1.max == side2.min){
                                
                            }else if(side1.min == side2.min && side1.max == side2.max){

                            }else if(MathUtils.epsLessThan(side1.min, side2.min) &&
                                     MathUtils.epsLessThan(side2.min, side1.max) &&
                                     MathUtils.epsLessThan(side1.max, side2.max)){

                            }else if(MathUtils.epsLessThan(side2.min, side1.min) &&
                                     MathUtils.epsLessThan(side1.min, side2.max) &&
                                     MathUtils.epsLessThan(side2.max, side1.max)){
                                
                            }
                        }
                    }
                }
            }else{
                return null;
            }
        }
        return null;
    }
}
