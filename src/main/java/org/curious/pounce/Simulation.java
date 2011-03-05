/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package org.curious.pounce;

import clojure.lang.APersistentMap;
import clojure.lang.IMapEntry;
import clojure.lang.IPersistentCollection;
import clojure.lang.IPersistentMap;
import clojure.lang.IPersistentSet;
import clojure.lang.ISeq;
import clojure.lang.ITransientCollection;
import clojure.lang.ITransientMap;
import clojure.lang.ITransientSet;
import clojure.lang.Keyword;
import clojure.lang.MapEntry;
import clojure.lang.PersistentHashMap;
import clojure.lang.PersistentHashSet;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.logging.Level;
import java.util.logging.Logger;

/**
 *
 * @author Jarl
 */
public class Simulation implements IPersistentMap {
    public IPersistentMap bodies;
    public IPersistentSet contacts;
    public IPersistentMap data;
    public static final Keyword bodiesKeyword;
    public static final Keyword contactsKeyword;
    public static final Keyword dieKeyword;

    static{
        bodiesKeyword = Keyword.intern("bodies");
        contactsKeyword = Keyword.intern("contacts");
        dieKeyword = Keyword.intern("die");
    }

    public static int nextId = 0;
    public static int uniqueId(){
        return nextId++;
    }

    public Simulation addBody(Body body){
        int id = uniqueId();
        IPersistentMap bodies = this.bodies.assoc(id, body);
        return fromData(data.assoc(bodiesKeyword, bodies));
    }

    public Simulation simulate(double delta){
        return simulate(delta, PersistentHashMap.EMPTY);
    }
    public Simulation simulate(double delta, IPersistentMap perturbations){

        ITransientCollection notDead = PersistentHashSet.EMPTY.asTransient();
        for(Object perturbation : perturbations){
            MapEntry entry = (MapEntry)perturbation;
            Object doDie = ((IPersistentMap)entry.val()).valAt(dieKeyword);
            if(doDie == null || !((Boolean)doDie).booleanValue()){
                notDead.conj(entry.val());
            }
        }
        IPersistentCollection bodies = notDead.persistent();

        Collision.detectCollisions(bodies);
        return null;
    }

    public Simulation(IPersistentMap bodies){
        this.bodies = bodies;
        contacts = PersistentHashSet.create();
        data = PersistentHashMap.create(bodiesKeyword, bodies, contactsKeyword, contacts);
    }

    public Simulation(IPersistentMap bodies, IPersistentSet contacts){
        this.bodies = bodies;
        this.contacts = contacts;
        data = PersistentHashMap.create(bodiesKeyword, bodies, contactsKeyword, contacts);
    }

    public Simulation fromData(IPersistentMap data){
        Simulation result = new Simulation((IPersistentMap)data.valAt(bodiesKeyword), (IPersistentSet)data.valAt(contactsKeyword));
        result.data = data;
        return result;
    }

    public IPersistentMap assoc(Object o, Object o1) {
        return fromData(this.data.assoc(o, o1));
    }

    public IPersistentMap assocEx(Object o, Object o1) throws Exception {
        if(containsKey(o)){
            throw new Exception("Key already present");
        }else{
            return assoc(o, o1);
        }
    }

    public IPersistentMap without(Object o) throws Exception {
        return fromData(this.data.without(o));
    }

    public Iterator iterator() {
        return data.iterator();
    }

    public boolean containsKey(Object o) {
        return data.containsKey(o);
    }

    public IMapEntry entryAt(Object o) {
        return data.entryAt(o);
    }

    public int count() {
        return data.count();
    }

    public IPersistentCollection empty() {
        return data.empty();
    }

    public ISeq seq() {
        return data.seq();
    }

    public Object valAt(Object o) {
        return data.valAt(o);
    }

    public Object valAt(Object o, Object o1) {
        return data.valAt(o, o1);
    }

    public IPersistentCollection cons(Object o) {
        return data.cons(o);
    }

    public boolean equiv(Object o) {
        return data.equiv(o);
    }

}
