#include <iostream>
#include <vector>

using namespace std;

int largest_poirot_seq(vector<int>, int, int, int, int, int);

int main() {
    int N;
    cin >> N;

    vector<int> lengths(N);
    for (int i = 0; i < N; i++)
        cin >> lengths[i]; // already sorted asc

    cout
        << largest_poirot_seq(lengths, -1, 0, -1, 0, 0)
        << endl;
}

int largest_poirot_seq(
    vector<int> lengths, int pre_i, int i,
    int seq_delta, int seq_length, int current_max
) {
    if (i == lengths.size()) return seq_length;
    int cigars_left = lengths.size() - i;
    if (seq_length + cigars_left <= current_max) return 0;

    if (seq_length == 0) {
        int max_with = largest_poirot_seq(lengths, i, i+1, -1, 1, current_max);
        current_max = max(current_max, max_with);
        int max_without = largest_poirot_seq(lengths, -1, i+1, -1, 0, current_max);
        return max(max_with, max_without);
    }
    else if (seq_length == 1) {
        int delta = lengths[i] - lengths[pre_i];
        int max_with = largest_poirot_seq(lengths, i, i+1, delta, 2, current_max);
        current_max = max(current_max, max_with);
        int max_without = largest_poirot_seq(lengths, pre_i, i+1, -1, 1, current_max);
        return max(max_with, max_without);
    }

    int this_delta = lengths[i] - lengths[pre_i];
    if (this_delta > seq_delta)
        return seq_length;
    else if (this_delta == seq_delta)
        return largest_poirot_seq(lengths, i, i+1, seq_delta, seq_length+1, current_max);
    else
        return largest_poirot_seq(lengths, pre_i, i+1, seq_delta, seq_length, current_max);
}
