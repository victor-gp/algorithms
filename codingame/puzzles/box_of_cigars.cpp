// https://www.codingame.com/training/medium/box-of-cigars

#include <iostream>
#include <vector>

using namespace std;

int main() {
    int N;
    cin >> N;

    vector<int> cigar_lengths(N);
    for (int i = 0; i < N; i++)
        cin >> cigar_lengths[i]; // already sorted asc

    // possible deltas are [0, cigar_max_length)
    int delta_upper_bound = cigar_lengths.back();
    // length of the sequence for every possible delta, at every cigar
    vector<vector<int>> seq_lengths(N, vector<int>(delta_upper_bound, 1));

    int poirot_number = 0;
    for (int i = 1; i < N; i++)
        for (int j = 0; j < i; j++) {
            int delta = cigar_lengths[i] - cigar_lengths[j];
            seq_lengths[i][delta] = seq_lengths[j][delta] + 1;
            if (seq_lengths[i][delta] > poirot_number)
                poirot_number = seq_lengths[i][delta];
        }

    cout << poirot_number << endl;
}
