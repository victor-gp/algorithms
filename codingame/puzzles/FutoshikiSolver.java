// https://www.codingame.com/ide/puzzle/futoshiki-solver
// https://en.wikipedia.org/wiki/Futoshiki

import java.util.*;
import java.io.*;

class Solution {

    public static void main(String args[]) {
        Futoshiki puzzle = new Futoshiki(System.in);
        puzzle.solve();
        System.out.println(puzzle);
    }
}

class Futoshiki {
    int size;
    Cell[][] grid;

    public Futoshiki(InputStream input) {
        Scanner in = new Scanner(input);

        size = in.nextInt(); // counting inequalities
        size = (size + 1) / 2; // only values
        grid = new Cell[size][size];
        in.nextLine();

        // all rows except for the last
        for (int i = 0; i < size-1; i++) {
            ValuesLineTuple t = scanValuesLine(size, in);

            StringTokenizer tokens = nextLineTokens(in);
            char[] downs  = new char[size];
            downs[0] = tokens.nextToken().charAt(0);
            for (int j = 1; j < size; j++) {
                tokens.nextToken(); // discard separator
                downs[j] = tokens.nextToken().charAt(0);
            }

            for (int j = 0; j < size; j++)
                grid[i][j] = new Cell(t.values[j], t.rights[j], downs[j]);
        }

        // last row (no downs)
        ValuesLineTuple t = scanValuesLine(size, in);
        for (int j = 0; j < size; j++)
            grid[size-1][j] = new Cell(t.values[j], t.rights[j], ' ');

        in.close();
    }

    private ValuesLineTuple scanValuesLine(int size, Scanner in) {
        StringTokenizer tokens = nextLineTokens(in);
        int[] values = new int[size];
        char[] rights = new char[size];

        values[0] = Integer.parseInt(tokens.nextToken());
        for (int j = 1; j < size; j++) {
            rights[j-1] = tokens.nextToken().charAt(0);
            values[j] = Integer.parseInt(tokens.nextToken());
        }
        rights[size-1] = ' ';

        ValuesLineTuple ret = new ValuesLineTuple();
        ret.values = values;
        ret.rights = rights;

        return ret;
    }

    private StringTokenizer nextLineTokens(Scanner in) {
        // tokenizer with delimiter = single space & delimiters are tokens
        return new StringTokenizer(in.nextLine(), " ", true);
    }

    private class ValuesLineTuple {
        public int[] values;
        public char[] rights;
    }

    public String toString() {
        String str = new String();
        for (int i = 0; i < size; i++) {
            for (int j = 0; j < size; j++)
                str += grid[i][j].toString();
            str += "\n";
        }
        return str;
    }

    // dumb backtracking
    public void solve() {}
}

class Cell {
    public int value;
    private Constraint right;
    private Constraint down;

    public Cell(int value, char right, char down) {
        this.value = value;
        this.right = Constraint.fromChar(right);
        this.down = Constraint.fromChar(down);
    }

    public String toString() {
        if (isSet())
            return String.valueOf(value);
        else
            return "?";
    }

    public boolean isSet() {
        return value != 0;
    }

    public boolean validateRight(int rightValue) {
        return right.validate(value, rightValue);
    }

    public boolean validateDown(int downValue) {
        return down.validate(value, downValue);
    }
}

// todo: rewrite as Inequality & enum InequalityType
abstract class Constraint {

    public static Constraint fromChar(char constraintChar) {
        switch (constraintChar) {
            case '>':
                return new More();
            case '<':
                return new Less();
            default:
                return new None();
        }
    }

    public abstract boolean validate(int value, int nextValue);
}

class None extends Constraint {

    public boolean validate(int value, int nextValue) {
        return true;
    }
}

class More extends Constraint {

    public boolean validate(int value, int nextValue) {
        return value > nextValue;
    }
}

class Less extends Constraint {

    public boolean validate(int value, int nextValue) {
        return value < nextValue;
    }
}
