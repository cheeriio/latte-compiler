// File defines built in functions
//
// printInt, printString, error, readInt, readString
// 
// and functions used by the compiler
//
// _copyString, _concatStrings

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

void printInt(long long x) {
    printf("%lld\n", x);
}

void printString(char* s) {
    puts(s);
}

void error() {
    puts("runtime error");
    exit(1);
}

long long readInt() {
    long long result;
    scanf("%lld  ", &result);
    return result;
}

char* readString() {
    char* res = NULL;
    size_t n = 0;
    getline(&res, &n, stdin);
    size_t len = strlen(res);
    if (len > 0 && res[len - 1] == '\n') {
        res[len - 1] = '\0';
    }
    return res;
}

// Below functions cause leaks

char* _copyString(const char* s) {
    int len = strlen(s) + 1;
    char* res = malloc(len);
    strcpy(res, s);
    return res;
}

char* _concatStrings(const char* s1, const char* s2) {
  const int len1 = strlen(s1);
  const int len = len1 + strlen(s2) + 1;
  char* res = malloc(len);
  strcpy(res, s1);
  strcpy(res + len1, s2);
  return res;
}

