/*
 * To change this template, choose Tools | Templates
 * and open the template in the editor.
 */

package mroscar;

/**
 *
 * @author jld
 */
import jpl.*;
import java.util.*;

public class PLCom {
	protected boolean simpleQuery(String q) {
		Query query = new Query(q);
		return query.hasSolution();
	}

	public PLCom() {
		simpleQuery("consult('oscares.pl')");
	}

	public void interrogate(String question) {
		if (question.endsWith(".") || question.endsWith("?"))
            question = question.substring(0, question.length()-1);
        simpleQuery("ln('" + question + "')");
	}

	public ArrayList getResult() {
		Variable X = new Variable("X");
        ArrayList solutions = new ArrayList();
		Query q = new Query("resultado", new Term[] {X});

        while (q.hasMoreSolutions()) {
            Hashtable solution = q.nextSolution();
            solutions.add(solution.get("X").toString());
        }

		return solutions;
	}
}
