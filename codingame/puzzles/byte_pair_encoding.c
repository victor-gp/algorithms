// https://www.codingame.com/training/medium/byte-pair-encoding

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

typedef char byte_pair[2];

struct production_rule
{
    char symbol;
    byte_pair replacement;
};

struct encoding
{
    char (*text)[];
    struct production_rule (*rules)[];
};

struct encoding encode(
    char text[], size_t text_size, struct production_rule rules[], char current_non_terminal);

int main()
{
    int n; // n chunks
    int m; // of length m
    scanf("%d%d", &n, &m);

    char text[n * m];
    for (int i = 0; i < n; i++)
        scanf("%s", &text[i * m]);

    int text_size = n * m;
    struct production_rule rules[0];
    struct encoding enc = encode(text, text_size, rules, 'Z');

    printf("%s\n", text);

    return 0;
}

bool equals(byte_pair a, byte_pair b)
{
    return memcmp(a, b, 2) == 0;
}

char *to_s(byte_pair bp)
{
    static char s[3];
    memcpy(s, bp, 2);
    s[3] = NULL;
    return &s;
}

struct pair_stats
{
    byte_pair pair;
    short freq;
    short first_index;
};

typedef struct pair_stats pair_stats;

const pair_stats NONE = {.pair = NULL, .freq = 0, .first_index = -1};

// returns stats for the most frequent byte pair
// leftmost one in case of tie, NONE if no repetitions
pair_stats most_frequent_pair(char text[], size_t text_size);

struct encoding encode(
    char text[], size_t text_size, struct production_rule rules[], char current_non_terminal)
{
    pair_stats to_be_replaced = most_frequent_pair(text, text_size);

    if (to_be_replaced.freq == 0)
    {
        printf("base case\n");
    }
    else
    {
        printf("%s\n", to_s(to_be_replaced.pair));
    }

    struct encoding ret_stub;
    return ret_stub;
}

// increases the freq of next_pair if present in pairs, grows pairs with it otherwise
void insert(pair_stats pairs[], size_t *pairs_size, byte_pair next_pair, size_t np_index);

// returns the most frequent pair_stats (leftmost if tie),
// or NONE if all pairs have a single occurrence
pair_stats max_repeated(pair_stats pairs[], size_t pairs_size);

// returns stats for the most frequent byte pair
// leftmost one in case of tie, NONE if no repetitions
pair_stats most_frequent_pair(char text[], size_t text_size)
{
    if (text_size < 4)
        return NONE;

    size_t max_possible_pairs = text_size - 1;
    pair_stats pairs[max_possible_pairs];
    size_t pairs_size;

    memcpy(pairs[0].pair, text, 2);
    pairs[0].freq = 1;
    pairs[0].first_index = 0;
    pairs_size = 1;

    byte_pair pre_bp;
    memcpy(pre_bp, text, 2);
    size_t pre_bp_i = 0;

    for (size_t i = 1; i + 1 < text_size; i++)
    {
        byte_pair bp;
        memcpy(bp, &text[i], 2);
        if (equals(bp, pre_bp) && i == pre_bp_i + 1)
            continue;
        insert(pairs, &pairs_size, bp, i);

        memcpy(pre_bp, bp, 2);
        pre_bp_i = i;
    }

    return max_repeated(pairs, pairs_size);
}

// increases the freq of next_pair if present in pairs, grows pairs with it otherwise
void insert(pair_stats pairs[], size_t *pairs_size, byte_pair next_pair, size_t np_index)
{
    // nice: optimize with alfabetical order + binary search

    for (size_t i = 0; i < *pairs_size; i++)
    {
        if (equals(pairs[i].pair, next_pair))
        {
            pairs[i].freq += 1;
            return;
        }
    }

    *pairs_size += 1;
    memcpy(pairs[*pairs_size].pair, next_pair, 2);
    pairs[*pairs_size].freq = 1;
    pairs[*pairs_size].first_index = np_index;
}

// returns the most frequent pair_stats (leftmost if tie),
// or NONE if all pairs have a single occurrence
pair_stats max_repeated(pair_stats pairs[], size_t pairs_size)
{
    pair_stats *max = pairs; // &pairs[0]
    for (size_t i = 1; i < pairs_size; i++)
    {
        if (pairs[i].freq > max->freq ||
            pairs[i].freq == max->freq && pairs[i].first_index < max->first_index)
        {
            max = &pairs[i];
        }
    }

    if (max->freq == 1)
        return NONE;
    else
        return *max;
}
