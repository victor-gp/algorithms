// https://www.codingame.com/ide/puzzle/futoshiki-solver
// https://en.wikipedia.org/wiki/Futoshiki

import java.util.*;
import java.io.*;
import java.math.*;

class Solution {

    public static void main(String args[]) {
        Scanner in = new Scanner(System.in);
        int size = in.nextInt();
        if (in.hasNextLine()) {
            in.nextLine();
        }
        for (int i = 0; i < size; i++) {
            String line = in.nextLine();
        }
        in.close();

        Futoshiki puzzle = new Futoshiki();
        puzzle.solve();

        System.out.println(puzzle);
    }
}

class Futoshiki {
    int size;
    Vector<Vector<Cell>> grid;

    public Futoshiki() {}

    public void solve() {}
    // dumb backtracking

    public String toString() {
        return "stub";
    }
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

    public boolean validateRight(int rightValue) {
        return right.validate(value, rightValue);
    }

    public boolean validateDown(int downValue) {
        return down.validate(value, downValue);
    }
}

abstract class Constraint {

    public static Constraint fromChar(char constraintChar) {
        return new None(); //todo
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
