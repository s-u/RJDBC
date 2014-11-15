package info.urbanek.Rpackage.RJDBC;

import java.math.BigDecimal;
import java.sql.Date;
import java.sql.ResultSet;
import java.sql.SQLException;
import java.sql.Time;
import java.sql.Timestamp;
import java.sql.Types;
import java.util.ArrayDeque;
import java.util.Arrays;

public class JDBCResultPull {
	private static enum RType {
		CT_STRING(1), CT_NUMERIC(2), CT_INTEGER(3), CT_BOOLEAN(4);

		final int type;

		private RType(int type) {
			this.type = type;
		}

		public int toInt() {
			return type;
		}

		public static RType fromValue(int sqlType) {
			switch (sqlType) {
			case Types.BIGINT:
			case Types.INTEGER:
			case Types.TINYINT:
			case Types.SMALLINT:
				return CT_INTEGER;

			case Types.BOOLEAN:
				return CT_BOOLEAN;

			case Types.DOUBLE:
			case Types.FLOAT:
			case Types.DECIMAL:
			case Types.NUMERIC:
			case Types.REAL:
				return CT_NUMERIC;

			default:
				return CT_STRING;
			}
		}
	}

	private static class ColumnType {
		private final RType rType;
		private final int sqlType;

		public ColumnType(int sqlType) {
			this.sqlType = sqlType;
			this.rType = RType.fromValue(sqlType);
		}

		public RType getRType() {return rType;}

		public int getSQLType() {return sqlType;}
	}

	ResultSet rs;
	ColumnType[] cTypes;
	Object[] data;
	int capacity;
	int count;
	int cols;

	ArrayDeque<int[]> nanIndex = new ArrayDeque<>();

	public JDBCResultPull(ResultSet rs) throws SQLException {
		this.rs = rs;
		cols = rs.getMetaData().getColumnCount();

		cTypes = new ColumnType[cols];
		for (int i=0; i<cols; i++) {
			cTypes[i] = new ColumnType(rs.getMetaData().getColumnType(i + 1));
		}

		data = new Object[cols];
		capacity = -1;
		count = 0;
	}

	public int[] getRType() {
		int[] types = new int[cols];
		for (int i=0; i<cols; i++) types[i] = getRType(i);
		return types;
	}
	
	public int getRType(int i) {
		return cTypes[i].getRType().toInt();
	}

	public int columns() { return cols; }

	public int count() { return count; }

	public void setCapacity(int atMost) {
		if (capacity != atMost) {
			for (int i = 0; i < cols; i++) switch(cTypes[i].getRType()) {
			case CT_NUMERIC:
				data[i] =new double[atMost];
				break;
			case CT_INTEGER:
				data[i] =new int[atMost];
				break;
			case CT_BOOLEAN:
				data[i] =new boolean[atMost];
				break;
			default:
				data[i] = new String[atMost];
				break;
			}
			capacity = atMost;
		}
	}

	public int fetch(int atMost) throws java.sql.SQLException {
		setCapacity(atMost);
		rs.setFetchSize(atMost);
		count = 0;
		if (!nanIndex.isEmpty()) nanIndex.clear();
		while (rs.next()) {
			for (int i = 0; i < cols; i++) {
				switch(cTypes[i].getSQLType()) {
				case Types.BIGINT:
				case Types.INTEGER:
				case Types.TINYINT:
				case Types.SMALLINT:
					((int[])data[i])[count] = rs.getInt(i + 1);
					break;
					
				case Types.BOOLEAN:
					((boolean[])data[i])[count] = rs.getBoolean(i + 1);
					break;

				case Types.DOUBLE:
				case Types.FLOAT:
				case Types.NUMERIC:
				case Types.REAL:
					((double[])data[i])[count] = rs.getDouble(i + 1);
					break;
					
				case Types.DECIMAL:
					BigDecimal tempDecimal = rs.getBigDecimal(i + 1);
					if (!rs.wasNull()) ((double[])data[i])[count] = tempDecimal.doubleValue();
					break;
					
				case Types.DATE:
					Date tempDate = rs.getDate(i + 1);
					if (!rs.wasNull()) ((String[])data[i])[count] = tempDate.toString();
					break;
					
				case Types.TIME:
					Time tempTime = rs.getTime(i + 1);
					if (!rs.wasNull()) ((String[])data[i])[count] = tempTime.toString();
					break;
					
				case Types.TIMESTAMP:
					Timestamp tempTimeStamp = rs.getTimestamp(i + 1);
					if (!rs.wasNull()) ((String[])data[i])[count] = tempTimeStamp.toString();
					break;

				default:
					((String[])data[i])[count] = rs.getString(i + 1);

				}
				if (rs.wasNull()) nanIndex.push(new int[] {count+1, i+1});
			}
			count++;
			if (count >= capacity) return count;
		}
		return count;
	}

	public int[] getNaNs() {
		int[] out = new int[2*nanIndex.size()];
		int counter = 0;
		while (!nanIndex.isEmpty()) {
			int[] current = nanIndex.pop();
			out[2*counter] = current[0];
			out[2*counter + 1] = current[1];
			counter++;
		}
		return out;
	}

	public Object getColumnData(int column) {
		return (column > 0 && column <= cols) ? data[column - 1] : null;
	}

	public String[] getStringCol(int column) {
		String[] contents = (String[]) data[column - 1];
		if (count == contents.length) return contents;
		return Arrays.copyOf(contents, count);
	}

	public double[] getNumericCol(int column) {
		double[] contents = (double[]) data[column - 1];
		if (count == contents.length) return contents;
		return Arrays.copyOf(contents, count);
	}

	public int[] getIntegerCol(int column) {
		int[] contents = (int[]) data[column - 1];
		if (count == contents.length) return contents;
		return Arrays.copyOf(contents, count);
	}

	public boolean[] getBooleanCol(int column) {
		boolean[] contents = (boolean[]) data[column - 1];
		if (count == contents.length) return contents;
		return Arrays.copyOf(contents, count);
	}
}
