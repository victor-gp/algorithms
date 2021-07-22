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

typedef struct production_rule production_rule;

struct encoding
{
    char (*text)[];
    production_rule (*rules)[];
};

typedef struct encoding encoding;

encoding encode(char text[], production_rule rules[], char current_non_terminal);

char *to_s(encoding e);

int main()
{
    int n; // n chunks
    int m; // of length m
    scanf("%d%d", &n, &m);

    char text[n * m + 1];
    for (int i = 0; i < n; i++)
        scanf("%s", &text[i * m]);
    text[n * m] = '\0';

    production_rule rules[0];
    encoding enc = encode(text, rules, 'Z');

    printf("%s\n", text);

    return 0;
}

bool equals(byte_pair a, byte_pair b)
{
    return memcmp(a, b, 2) == 0;
}

char *bp_to_s(byte_pair bp)
{
    static char s[3];
    memcpy(s, bp, 2);
    s[2] = '\0';
    return s;
}

char *pr_to_s(production_rule pr)
{
    static char s[6];
    sprintf(s, "%c = %s", pr.symbol, bp_to_s(pr.replacement));
    return s;
}

char *to_s(encoding e);

struct pair_stats
{
    size_t first_index;
    byte_pair pair;
    short freq;
};

typedef struct pair_stats pair_stats;

// returns stats for the most frequent byte pair
// leftmost one in case of tie, NONE if no repetitions
pair_stats most_frequent_pair(char text[]);

// replaces all instances of rule.replacement for rule.symbol, left to right
void gsub(char text[], production_rule rule);

encoding encode(char text[], production_rule rules[], char current_non_terminal)
{
    pair_stats to_be_replaced = most_frequent_pair(text);

    if (to_be_replaced.freq == 0)
    {
        printf("base case\n");
        encoding ret_stub;
        return ret_stub;
    }

    production_rule new_rule;
    new_rule.symbol = current_non_terminal;
    memcpy(new_rule.replacement, to_be_replaced.pair, 2);
    --current_non_terminal;
    printf("%s\n", pr_to_s(new_rule));

    // todo: append new rule to rules;

    gsub(text, new_rule);
    printf("%s\n", text);

    encode(text, rules, current_non_terminal);
}

const pair_stats NONE = {.pair = {'\0', '\0'}, .freq = 0, .first_index = -1};

// increases the freq of pair if already in pairs, adds it into pairs otherwise
void accumulate(pair_stats pairs[], size_t *pairs_size, byte_pair pair, size_t pair_index);

// returns the most frequent pair_stats (leftmost if tie),
// or NONE if all pairs have a single occurrence
pair_stats max_repeated(pair_stats pairs[], size_t pairs_size);

// returns stats for the most frequent byte pair
// leftmost one in case of tie, NONE if no repetitions
pair_stats most_frequent_pair(char text[])
{
    size_t text_size = strlen(text);
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

        accumulate(pairs, &pairs_size, bp, i);
        memcpy(pre_bp, bp, 2);
        pre_bp_i = i;
    }

    return max_repeated(pairs, pairs_size);
}

// increases the freq of pair if already in pairs, adds it into pairs otherwise
void accumulate(pair_stats pairs[], size_t *pairs_size, byte_pair pair, size_t pair_index)
{
    // nice: optimize with alfabetical order + binary search

    for (size_t i = 0; i < *pairs_size; i++)
    {
        if (equals(pairs[i].pair, pair))
        {
            pairs[i].freq += 1;
            return;
        }
    }

    memcpy(pairs[*pairs_size].pair, pair, 2);
    pairs[*pairs_size].freq = 1;
    pairs[*pairs_size].first_index = pair_index;
    *pairs_size += 1;
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

// replaces all instances of rule.replacement for rule.symbol, left to right
void gsub(char text[], production_rule rule)
{
    size_t i, j;
    for (i = 0, j = i; text[j] != '\0'; i++, j++)
    {
        byte_pair current;
        memcpy(current, &text[j], 2);
        if (equals(current, rule.replacement))
        {
            text[i] = rule.symbol;
            ++j; // skip the second byte of the pair
        }
        else
        {
            text[i] = text[j];
        }
    }
    text[i] = '\0';
    // todo: free mem
}
