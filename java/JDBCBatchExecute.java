package info.urbanek.Rpackage.RJDBC;

import java.sql.PreparedStatement;

public class JDBCBatchExecute {
    Object[] cache;
    int[]    types;
    int      ptr;
    long     tl;
    PreparedStatement s;
    
    static final int T_DOUBLE = 1;
    static final int T_INT    = 2;
    static final int T_STRING = 3;
    
    public JDBCBatchExecute(PreparedStatement stat, int n) {
	cache = new Object[n];
	types = new int[n];
	s = stat;
	ptr = 0;
	tl = 0;
    }

    public void addDoubles(double[] d) {
	tl = d.length;
	types[ptr] = T_DOUBLE;
	cache[ptr++] = d;
    }

    public void addIntegers(int[] i) {
	tl = i.length;
	types[ptr] = T_INT;
	cache[ptr++] = i;
    }

    public void addStrings(String[] s) {
	tl = s.length;
	types[ptr] = T_STRING;
	cache[ptr++] = s;
    }

    public void execute(int maxBatch) throws java.sql.SQLException {
	if (ptr > 0) {
	    int bptr = 0;
	    int l = 0;
	    while (l < tl) {
		int i = 0;
		while (i < ptr) {
		    if (types[i] == T_DOUBLE) {
			double val = ((double[])(cache[i]))[l];
			if (Double.isNaN(val))
			    s.setNull(i + 1, java.sql.Types.DOUBLE);
			else
			    s.setDouble(i + 1, val);
		    } else if (types[i] == T_INT) {
			int val = ((int[])(cache[i]))[l];
			if (val == Integer.MIN_VALUE)
			    s.setNull(i + 1, java.sql.Types.INTEGER);
			else
			    s.setInt(i + 1, val);
		    } else if(types[i] == T_STRING) {
			String val = ((String[])(cache[i]))[l];
			if (val == null)
			    s.setNull(i + 1, java.sql.Types.VARCHAR);
			else
			    s.setString(i + 1, val);
		    }
		    i++;
		}
		s.addBatch();
		l++;
		bptr++;
		if (maxBatch > -1 && bptr >= maxBatch) {
		    bptr = 0;
		    s.executeBatch();
		}		    
	    }
	}
	s.executeBatch();
    }
}
