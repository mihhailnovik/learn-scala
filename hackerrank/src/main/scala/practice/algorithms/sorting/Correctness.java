package practice.algorithms.sorting;

public class Correctness {

    public static void insertionSort(int[] a) {
        for (int i = 1; i < a.length; i++) {
            int value = a[i];
            int j = i - 1;
            while (j >= 0 && a[j] > value) {
                a[j + 1] = a[j];
                printArray(a);
                j = j - 1;
            }
            a[j + 1] = value;
            printArray(a);
        }
        printArray(a);
    }


    static void printArray(int[] ar) {
        for (int n : ar) {
            System.out.print(n + " ");
        }
        System.out.println();
    }

    public static void main(String[] args) {
        int[] ar = new int[]{7, 4, 3, 5, 6, 2};
        insertionSort(ar);
    }
}