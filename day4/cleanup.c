#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>

typedef struct assignment_pair assignment_pair;

struct assignment_pair {
    int min1;
    int max1;

    int min2;
    int max2;

    assignment_pair* next;
};

assignment_pair* read_pairs(char* file) {
    FILE * fp = fopen(file, "r");
    int min1, max1, min2, max2;
    assignment_pair* curr = (assignment_pair*)malloc(sizeof(assignment_pair));
    assignment_pair* first = curr;


    char line[256];
    while (fgets(line, sizeof(line), fp))
    {
        // Use sscanf to parse the values from the line
        sscanf(line, "%i-%i,%i-%i", &min1, &max1, &min2, &max2);

        curr->min1 = min1;
        curr->max1 = max1;
        curr->min2 = min2;
        curr->max2 = max2;

        // Save current file position and check if we're at the last line of the input file
        long pos = ftell(fp);
        if (!fgets(line, sizeof(line), fp)) {
            // If we are, set the next pointer of the current struct to NULL
            // to indicate the end of the list
            curr->next = NULL;
            break;
        }

        // Return to saved position and allocate next struct
        fseek(fp, pos, SEEK_SET);
        curr->next = (assignment_pair*)malloc(sizeof(assignment_pair));
        curr = curr->next;
    }

    fclose(fp);
    return first;
}

void print_pairs(assignment_pair* pairs) {
    while (pairs != NULL) {
        printf("%i-%i,%i-%i\n", pairs->min1, pairs->max1, pairs->min2, pairs->max2);
        pairs = pairs->next;
    }
}

void free_pairs(assignment_pair* pairs) {
    assignment_pair* curr = pairs;

    while (curr != NULL)
    {
        assignment_pair* next = curr->next;
        free(curr);
        curr = next;
    }
}

bool is_fully_overlapping(int min1, int max1, int min2, int max2) {
    bool one_covers_two = min1 <= min2 && max1 >= max2;
    bool two_covers_one = min2 <= min1 && max2 >= max1;

    return one_covers_two || two_covers_one;
}

int count_fully_overlapping_pairs(assignment_pair* pairs) {
    assignment_pair* curr = pairs;
    int count = 0;

    while (curr != NULL)
    {
        if (is_fully_overlapping(curr->min1, curr->max1, curr->min2, curr->max2)) { count++; }
        curr = curr->next;
    }

    return count;
}

bool is_overlapping(int min1, int max1, int min2, int max2) {
    bool case1 = min2 <= max1 && min2 >= min1; // min2 falls within [min1, max1]
    bool case2 = max2 <= max1 && max2 >= min1; // max2 falls within [min1, max1]
    bool case3 = min1 <= max2 && min1 >= min2; // min1 falls within [min2, max2]
    bool case4 = max1 <= max2 && max1 >= min2; // max1 falls within [min2, max2]

    return case1 || case2 || case3 || case4;
}

int count_overlapping_pairs(assignment_pair* pairs) {
    assignment_pair* curr = pairs;
    int count = 0;

    while (curr != NULL)
    {
        if (is_overlapping(curr->min1, curr->max1, curr->min2, curr->max2)) { count++; }
        curr = curr->next;
    }

    return count;
}

int main(void) {
    assignment_pair* pairs = read_pairs("input.txt");
    //print_pairs(pairs);

    printf("number of fully overlapping pairs: %i\n", count_fully_overlapping_pairs(pairs));
    printf("number of overlapping pairs: %i\n", count_overlapping_pairs(pairs));

    free_pairs(pairs);
}