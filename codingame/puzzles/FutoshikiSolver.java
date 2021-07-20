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
    Inequality right;
    Inequality down;

    public Cell(int value, char right, char down) {
        this.value = value;
        this.right = new Inequality(right);
        this.down = new Inequality(down);
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

class Inequality {
    Type type;

    private enum Type {
        MORE, LESS, NONE
    }

    public Inequality(char constraintChar) {
        switch (constraintChar) {
            case '>':
                type = Type.MORE;
                break;
            case '<':
                type = Type.LESS;
                break;
            default:
                type = Type.NONE;
        }
    }

    public boolean validate(int value, int nextValue) {
        switch (type) {
            case MORE:
                return value > nextValue;
            case LESS:
                return value < nextValue;
            default:
                return true;
        }
    }
}
