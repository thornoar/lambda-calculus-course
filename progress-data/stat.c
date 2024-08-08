#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

int main () {
    int lim = 1;
    int num_of_questions = 27;
    int num_of_students = 14;
    int num_of_sheets = 2;
    int num_of_problems_to_pass = 3;
    int max_num_of_problems = 25;
    int total_num_of_problems = 45;

    FILE* problems_csv[num_of_sheets];
    problems_csv[0] = fopen("../progress-data/problems-1.csv", "r");
    problems_csv[1] = fopen("../progress-data/problems-2.csv", "r");

    char names[num_of_students][128];
    float scores[num_of_students][num_of_sheets][max_num_of_problems];

    printf("| \e[34mInitialized databases.\e[0m\n"); // ]]

    for (int i = 0; i < num_of_sheets; i++) {
        char dump[4096];
        fgets(dump, sizeof(dump), problems_csv[i]);

        for (int j = 0; j < num_of_students; j++) {
            char whole_string[248];
            fgets(whole_string, sizeof(whole_string), problems_csv[i]);

            char* token = strtok(whole_string, ",");

            if (i == 0) strcpy(names[j], token);
            token = strtok(NULL, ",");

            int index = 0;
            while (token != NULL) {
                char* pend;
                scores[j][i][index] = strtof(token, &pend);

                token = strtok(NULL, ",");
                index++;
            }
        }
    }

    printf("| \e[34mFilled databases.\e[0m\n"); // ]]

    int l1 = 17;
    int l2 = 15;

    char* fst = "Name";
    printf("%s", fst);
    for (int j = 0; j < l1 - strlen(fst); j++) { printf(" "); }
    char* snd = "part 1";
    printf("%s", snd);
    for (int j = 0; j < l2 - strlen(fst); j++) { printf(" "); }
    char* thd = "part 2";
    printf("%s\n", thd);

    for (int i = 0; i < num_of_students; i++) {
        printf("%s", names[i]);
        int s1 = l1 - (int)(strlen(names[i])/2);
        for (int j = 0; j < s1; j++) { printf(" "); }
        for (int j = 0; j < num_of_sheets; j++) {
            int not_admitted = 0;
            int count = 0;
            float score = 0.0;
            for (int k = 0; k < max_num_of_problems; k++) {
                if (scores[i][j][k] > 0) {
                    count++;
                    score += scores[i][j][k];
                }
            }
            if (count < num_of_problems_to_pass) {
                // strcpy(not_passed[not_passed_index], names[i]);
                not_admitted = 1;
                // not_passed_index++;
            }
            printf("%f", score);
            int s2 = l2 - 6;
            if (score > 10.0) { s2--; }
            if (not_admitted == 1) {
                printf(" *");
                s2 -= 2;
            }
            for (int j = 0; j < s2; j++) { printf(" "); }
        }
        printf("\n");
    }

    // printf("\n");
    // printf("List of students not admitted:\n");
    //
    // int size = 0;
    //
    // for (int i = 0; i < sizeof(not_passed)/sizeof(not_passed[0]); i++) {
    //     printf("%s\n", not_passed[i]);
    //     size = i;
    // }

    // printf("\n");
    // printf("total: %i people\n", size);

    return 0;
}
