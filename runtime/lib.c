#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdbool.h>


void printInt(int n) {
    printf("%d\n", n);
}

int readInt() {
    int n;
    scanf("%d\n", &n);
    return n;
}

void printString(char* s) {
    printf("%s\n", s);
}

char* readString() {
    size_t n = 255;
    char* s = (char*) malloc(n * sizeof(char));
    fgets(s, n, stdin);
    s[strlen(s) - 1] = '\0';
    return s;
}

char* concatStrings(char* s1, char* s2) {
    size_t n = strlen(s1) + strlen(s2);
    char* s3 = (char*) malloc((n + 1) * sizeof(char));
    strcpy(s3, s1);
    strcat(s3, s2);
    return s3;
}

bool compareStrings(char* s1, char* s2) {
    return strcmp(s1, s2);
}
