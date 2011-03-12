package org.curious.pounce;

import clojure.lang.IFn;
import clojure.lang.IPersistentMap;
import clojure.lang.Keyword;
import clojure.lang.PersistentHashMap;
import clojure.lang.RT;
import clojure.lang.Var;
import java.awt.Graphics;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.util.LinkedList;
import java.util.List;
import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.Timer;
import org.curious.pounce.math.Vector;

/**
 * Hello world!
 *
 */
public class App 
{
    public static Simulation simulation;
    public static JFrame frame;

    public static void main( String[] args ) throws Exception
    {
        List<Vector> boxVectors = new LinkedList<Vector>();
        boxVectors.add(new Vector(0, 0));
        boxVectors.add(new Vector(50, 0));
        boxVectors.add(new Vector(50, 50));
        boxVectors.add(new Vector(0, 50));

        List<Shape> boxShape = new LinkedList<Shape>();
        boxShape.add(new Polygon(1, boxVectors));

        List<Vector> groundVectors = new LinkedList<Vector>();
        boxVectors.add(new Vector(0, 0));
        boxVectors.add(new Vector(500, 0));
        boxVectors.add(new Vector(500, 500));
        boxVectors.add(new Vector(0, 500));

        List<Shape> groundShape = new LinkedList<Shape>();
        groundShape.add(new Polygon(groundVectors));

        IPersistentMap bodies = PersistentHashMap.create(Keyword.intern("box"), new Body(boxShape),
                                                         Keyword.intern("ground"), new Body(groundShape));
        simulation = new Simulation(bodies);
        new Timer(16, new Updater()).start();

        frame = new JFrame();
        frame.setContentPane(new JPanel(){
            @Override
            public void paintComponent(Graphics graphics){
                super.paintComponent(graphics);
                simulation.render(graphics);
            }
        });
        frame.setSize(500, 500);
        frame.setLocationRelativeTo(null);
        frame.setVisible(false);
    }

    public static class Updater implements ActionListener{
        public void actionPerformed(ActionEvent ae) {
            simulation = simulation.simulate(1/60);
        }
    }

    public static class Renderer implements ActionListener{
        public void actionPerformed(ActionEvent ae) {
            frame.repaint();
        }
    }
}
