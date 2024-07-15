#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <math.h>

void close () {
    printf("exiting...\n");
    exit(0);
}

int main () {
    int lim = 1;
    int num_of_questions = 27;
    int num_of_students = 15;
    int num_of_sheets = 2;
    int num_of_problems_to_pass = 5;
    int num_of_problems = 20;

    FILE* problems_csv[num_of_sheets];
    problems_csv[0] = fopen("../progress-data/problems-1.csv", "r");
    problems_csv[1] = fopen("../progress-data/problems-2.csv", "r");

    char names[num_of_students][128];
    float scores[num_of_students][num_of_sheets][num_of_problems];

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

    while (1) {
        printf("(\e[33mSTUDENT NAME\e[0m): "); // ]]
        char name[256];
        fgets(name, sizeof(name), stdin);
        size_t len = strlen(name);
        if (len > 0 && name[len-1] == '\n') {
            name[--len] = '\0';
        }

        if (feof(stdin) || strcmp(name, "quit\n") == 0) {
            close();
        }

        int found = 0;
        float score = 0.0;
        for (int i = 0; i < sizeof(names)/sizeof(*names); i++) {
            if (strcmp(name, names[i]) == 0) {
                found = 1;
                for (int j = 0; j < num_of_sheets; j++) {
                    int count = 0;
                    for (int k = 0; k < num_of_problems; k++) {
                        if (scores[i][j][k] > 0) {
                            count++;
                            score += scores[i][j][k];
                        }
                    }
                    if (count < num_of_problems_to_pass) {
                        printf(
                            "Not enough problems solved on sheet \e[33m%i\e[0m! Only \e[33m%i\e[0m out of \e[33m%i\e[0m!\n", // ]]]
                            j+1,
                            count,
                            num_of_problems_to_pass
                        );
                        goto nextprompt;
                    }
                }
            }
        }
        if (found == 0) {
            printf("No student with name \e[33m%s\e[0m!\n", name); // ]]
            goto nextprompt;
        }

        printf("Total score of student: %f\n", score);

        float random = (float)rand()/((float)RAND_MAX/(float)num_of_questions);

nextprompt:
        continue;
    }

    return 0;
}
