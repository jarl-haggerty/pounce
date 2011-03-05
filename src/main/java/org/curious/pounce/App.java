package org.curious.pounce;

import clojure.lang.IFn;
import clojure.lang.IPersistentMap;
import clojure.lang.Keyword;
import clojure.lang.PersistentHashMap;
import clojure.lang.RT;
import clojure.lang.Var;
import org.curious.pounce.math.Vector;

/**
 * Hello world!
 *
 */
public class App 
{
    public static void main( String[] args ) throws Exception
    {
        Keyword keyword = Keyword.intern("bodies");
        IPersistentMap map = PersistentHashMap.create(1, 2, 3, 4, keyword, 6);
        System.out.println(keyword);
        System.out.println(map);
        System.out.println(keyword.invoke(map));
    }
}
