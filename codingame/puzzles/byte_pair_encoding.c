// https://www.codingame.com/ide/puzzle/byte-pair-encoding

#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdbool.h>

typedef char byte_pair[2];

struct production_rule {
    char symbol;
    byte_pair sub;
};

struct pair_freq {
    byte_pair pair;
    uint freq;
};

struct encoding {
    char (*text)[];
    struct production_rule (*rules)[];
};

struct encoding encode(char text[], struct production_rule rules[], char current_non_terminal);

int main() {
    int n; // n lines
    int m; // of length m
    scanf("%d%d", &n, &m);

    char text[n*m];
    for (int i = 0; i < n; i++)
        scanf("%s", &text[i*m]);

    struct production_rule rules[0];
    struct encoding enc = encode(text, rules, 'Z');

    printf("%s", text);

    return 0;
}

struct encoding encode(char text[], struct production_rule rules[], char current_non_terminal) {
    struct encoding enc;
    return enc;
}
