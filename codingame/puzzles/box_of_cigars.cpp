#include <iostream>
#include <vector>

using namespace std;

int largest_poirot_seq(vector<int>, int, int, int);

int main() {
    int N;
    cin >> N;

    vector<int> lengths(N);
    for (int i = 0; i < N; i++)
        cin >> lengths[i]; // already sorted asc

    cout
        << largest_poirot_seq(lengths, -1, 0, -1)
        << endl;
}

int largest_poirot_seq(
    vector<int> lengths, int pre_i, int i, int delta
) {
    if (i == lengths.size()) return 0;

    if (pre_i == -1) {
        int max_with = 1 + largest_poirot_seq(lengths, i, i+1, -1);
        int max_without = largest_poirot_seq(lengths, -1, i+1, -1);
        return max(max_with, max_without);
    }
    else if (delta == -1) {
        int delta = lengths[i] - lengths[pre_i];
        int max_with = 1 + largest_poirot_seq(lengths, i, i+1, delta);
        int max_without = largest_poirot_seq(lengths, pre_i, i+1, -1);
        return max(max_with, max_without);
    }

    bool matches = delta == lengths[i] - lengths[pre_i];
    if (matches)
        return 1 + largest_poirot_seq(lengths, i, i+1, delta);
    else
        return largest_poirot_seq(lengths, pre_i, i+1, delta);
}
