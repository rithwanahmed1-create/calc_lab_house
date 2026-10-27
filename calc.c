// Rithwan Ahmed Jabarullah Khan 241ADB143
// https://github.com/rithwanahmed1-create/calc_lab_house
// Compile with: gcc -O2 -Wall -Wextra -std=c17 -o calc calc.c -lm
// Resources used: GitHub Copilot, prompt: "Implement the requested arithmetic parser in C using the supplied lab requirements." Conversation history URL: unavailable in this workspace; add a shareable link before submission.

#include <ctype.h>
#include <dirent.h>
#include <errno.h>
#include <limits.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>

#define STUDENT_FIRST "rithwan"
#define STUDENT_LAST "khan"
#define STUDENT_ID "241ADB143"

typedef struct {
    int is_float;
    long long integer;
    double real;
} Value;

typedef struct {
    Value value;
    size_t start;
    size_t operator_position;
} PowerItem;

typedef struct {
    /* Absolute byte offsets include skipped comments and newline bytes. */
    const char *text;
    size_t position;
    size_t line_end;
    size_t error_position;
    unsigned int nesting;
    int allocation_failed;
} Parser;

typedef struct {
    char **items;
    size_t count;
    size_t capacity;
} StringList;

static Value parse_expression(Parser *parser);

static Value integer_value(long long value)
{
    Value result = {0, value, (double)value};
    return result;
}

static Value float_value(double value)
{
    Value result = {1, 0, value};
    return result;
}

static void fail_at(Parser *parser, size_t position)
{
    /* Parsing advances left to right, so retaining the first failure identifies its token. */
    if (parser->error_position == 0) {
        parser->error_position = position;
    }
}

static void skip_spaces(Parser *parser)
{
    while (parser->position < parser->line_end) {
        char character = parser->text[parser->position];
        if (character != ' ' && character != '\t' && character != '\r') {
            break;
        }
        parser->position++;
    }
}

static int checked_add(long long left, long long right, long long *result)
{
    if ((right > 0 && left > LLONG_MAX - right) ||
        (right < 0 && left < LLONG_MIN - right)) {
        return 0;
    }
    *result = left + right;
    return 1;
}

static int checked_subtract(long long left, long long right, long long *result)
{
    if ((right < 0 && left > LLONG_MAX + right) ||
        (right > 0 && left < LLONG_MIN + right)) {
        return 0;
    }
    *result = left - right;
    return 1;
}

static int checked_multiply(long long left, long long right, long long *result)
{
    if (left == 0 || right == 0) {
        *result = 0;
        return 1;
    }
    if ((left == -1 && right == LLONG_MIN) ||
        (right == -1 && left == LLONG_MIN)) {
        return 0;
    }
    if (left > 0) {
        if ((right > 0 && left > LLONG_MAX / right) ||
            (right < 0 && right < LLONG_MIN / left)) {
            return 0;
        }
    } else if ((right > 0 && left < LLONG_MIN / right) ||
               (right < 0 && left < LLONG_MAX / right)) {
        return 0;
    }
    *result = left * right;
    return 1;
}

static int integer_power(long long base, long long exponent, long long *result)
{
    long long product = 1;
    long long factor = base;

    while (exponent > 0) {
        if ((exponent & 1LL) != 0 &&
            !checked_multiply(product, factor, &product)) {
            return 0;
        }
        exponent >>= 1;
        if (exponent > 0 && !checked_multiply(factor, factor, &factor)) {
            return 0;
        }
    }
    *result = product;
    return 1;
}

static Value apply_operator(Parser *parser, char operation,
                            Value left, Value right,
                            size_t operator_position, size_t right_start)
{
    long long integer_result;
    /* Preserve integer division and exact integer powers unless a float participates. */
    int use_float = left.is_float || right.is_float;

    if (operation == '/' &&
        ((right.is_float && right.real == 0.0) ||
         (!right.is_float && right.integer == 0))) {
        /* Division's invalid value is the right operand, not the slash. */
        fail_at(parser, right_start + 1);
        return integer_value(0);
    }

    if (operation == '*' && right.is_float == 0 && left.is_float == 0) {
        if (!checked_multiply(left.integer, right.integer, &integer_result)) {
            fail_at(parser, operator_position + 1);
            return integer_value(0);
        }
        return integer_value(integer_result);
    }
    if (operation == '+' && !use_float) {
        if (!checked_add(left.integer, right.integer, &integer_result)) {
            fail_at(parser, operator_position + 1);
            return integer_value(0);
        }
        return integer_value(integer_result);
    }
    if (operation == '-' && !use_float) {
        if (!checked_subtract(left.integer, right.integer, &integer_result)) {
            fail_at(parser, operator_position + 1);
            return integer_value(0);
        }
        return integer_value(integer_result);
    }
    if (operation == '/' && !use_float) {
        if (left.integer == LLONG_MIN && right.integer == -1) {
            fail_at(parser, operator_position + 1);
            return integer_value(0);
        }
        return integer_value(left.integer / right.integer);
    }

    if (operation == '+' || operation == '-' || operation == '*' ||
        operation == '/') {
        double left_number = left.is_float ? left.real : (double)left.integer;
        double right_number = right.is_float ? right.real : (double)right.integer;
        double result;

        if (operation == '+') {
            result = left_number + right_number;
        } else if (operation == '-') {
            result = left_number - right_number;
        } else if (operation == '*') {
            result = left_number * right_number;
        } else {
            result = left_number / right_number;
        }
        if (!isfinite(result)) {
            /* A non-finite floating result is attributed to the operation that created it. */
            fail_at(parser, operator_position + 1);
            return integer_value(0);
        }
        return float_value(result);
    }

    if (!use_float) {
        if (right.integer < 0) {
            /* The exponent's primary begins at right_start, including an opening parenthesis. */
            fail_at(parser, right_start + 1);
            return integer_value(0);
        }
        if (!integer_power(left.integer, right.integer, &integer_result)) {
            fail_at(parser, operator_position + 1);
            return integer_value(0);
        }
        return integer_value(integer_result);
    }

    {
        double left_number = left.is_float ? left.real : (double)left.integer;
        double right_number = right.is_float ? right.real : (double)right.integer;
        double result = pow(left_number, right_number);

        if (!isfinite(result)) {
            /* A non-finite power result is attributed to its operator. */
            fail_at(parser, operator_position + 1);
            return integer_value(0);
        }
        return float_value(result);
    }
}

static Value parse_primary(Parser *parser)
{
    size_t start;
    char character;

    skip_spaces(parser);
    if (parser->position >= parser->line_end) {
        fail_at(parser, parser->line_end + 1);
        return integer_value(0);
    }

    start = parser->position;
    character = parser->text[parser->position];
    if (character == '(') {
        Value result;

        if (parser->nesting >= 1000) {
            fail_at(parser, start + 1);
            return integer_value(0);
        }
        parser->nesting++;
        parser->position++;
        result = parse_expression(parser);
        skip_spaces(parser);
        if (parser->error_position == 0) {
            if (parser->position >= parser->line_end) {
                /* The missing close parenthesis is discovered at the line boundary. */
                fail_at(parser, parser->line_end + 1);
            } else if (parser->text[parser->position] != ')') {
                fail_at(parser, parser->position + 1);
            } else {
                parser->position++;
            }
        }
        parser->nesting--;
        return result;
    }

    if (isdigit((unsigned char)character)) {
        size_t digits_end;
        int is_float = 0;
        char *end_pointer;

        while (parser->position < parser->line_end &&
               isdigit((unsigned char)parser->text[parser->position])) {
            parser->position++;
        }
        digits_end = parser->position;
        if (parser->position < parser->line_end &&
            parser->text[parser->position] == '.') {
            is_float = 1;
            parser->position++;
            if (parser->position >= parser->line_end ||
                !isdigit((unsigned char)parser->text[parser->position])) {
                /* A decimal point must have digits on both sides. */
                fail_at(parser, digits_end + 1);
                return integer_value(0);
            }
            while (parser->position < parser->line_end &&
                   isdigit((unsigned char)parser->text[parser->position])) {
                parser->position++;
            }
            if (parser->position < parser->line_end &&
                (parser->text[parser->position] == 'e' ||
                 parser->text[parser->position] == 'E')) {
                fail_at(parser, parser->position + 1);
                return integer_value(0);
            }
        }

        errno = 0;
        if (is_float) {
            double number = strtod(parser->text + start, &end_pointer);
            if ((size_t)(end_pointer - parser->text) != parser->position ||
                !isfinite(number)) {
                fail_at(parser, start + 1);
                return integer_value(0);
            }
            return float_value(number);
        } else {
            long long number = strtoll(parser->text + start, &end_pointer, 10);
            if ((size_t)(end_pointer - parser->text) != parser->position ||
                errno == ERANGE) {
                fail_at(parser, start + 1);
                return integer_value(0);
            }
            return integer_value(number);
        }
    }

    fail_at(parser, start + 1);
    return integer_value(0);
}

// AI (GitHub Copilot), prompt: "Implement the recursive-descent grammar, precedence, and exact error positions from the supplied lab." Conversation history URL: unavailable in this workspace.
static Value parse_power(Parser *parser)
{
    size_t count = 0;
    size_t capacity = 4;
    PowerItem *items;
    Value result;
    Value first = parse_primary(parser);

    /* Most expressions have no power operator, so they need no auxiliary storage. */
    if (parser->error_position != 0) {
        return first;
    }
    skip_spaces(parser);
    if (parser->position + 1 >= parser->line_end ||
        parser->text[parser->position] != '*' ||
        parser->text[parser->position + 1] != '*') {
        return first;
    }

    items = malloc(capacity * sizeof(*items));
    if (items == NULL) {
        parser->allocation_failed = 1;
        return first;
    }

    /* Collect the chain and fold from the right to enforce ** right-associativity. */
    items[count].value = first;
    items[count].start = parser->position;
    items[count].operator_position = 0;
    count++;

    while (parser->error_position == 0) {
        size_t operator_position;
        size_t operand_start;

        skip_spaces(parser);
        if (parser->position + 1 >= parser->line_end ||
            parser->text[parser->position] != '*' ||
            parser->text[parser->position + 1] != '*') {
            break;
        }
        operator_position = parser->position;
        parser->position += 2;
        skip_spaces(parser);
        if (parser->position >= parser->line_end) {
            /* The newline (or EOF position) is where a missing operand becomes certain. */
            fail_at(parser, parser->line_end + 1);
            break;
        }
        operand_start = parser->position;
        items[count - 1].operator_position = operator_position;

        if (count == capacity) {
            size_t new_capacity = capacity * 2;
            PowerItem *new_items = realloc(items, new_capacity * sizeof(*items));
            if (new_items == NULL) {
                parser->allocation_failed = 1;
                break;
            }
            items = new_items;
            capacity = new_capacity;
        }
        items[count].value = parse_primary(parser);
        items[count].start = operand_start;
        items[count].operator_position = 0;
        count++;
    }

    if (parser->allocation_failed) {
        free(items);
        return integer_value(0);
    }
    result = items[count - 1].value;
    while (count > 1 && parser->error_position == 0) {
        size_t index = count - 1;
        result = apply_operator(parser, '^', items[index - 1].value, result,
                                items[index - 1].operator_position,
                                items[index].start);
        count--;
    }
    free(items);
    return result;
}

static Value parse_term(Parser *parser)
{
    Value result = parse_power(parser);

    while (parser->error_position == 0) {
        size_t operator_position;
        size_t right_start;
        char operation;
        Value right;

        skip_spaces(parser);
        if (parser->position >= parser->line_end) {
            break;
        }
        operation = parser->text[parser->position];
        if (operation != '*' && operation != '/') {
            break;
        }
        operator_position = parser->position++;
        skip_spaces(parser);
        if (parser->position >= parser->line_end) {
            /* The newline (or EOF position) is where a missing operand becomes certain. */
            fail_at(parser, parser->line_end + 1);
            break;
        }
        right_start = parser->position;
        right = parse_power(parser);
        if (parser->error_position == 0) {
            result = apply_operator(parser, operation, result, right,
                                    operator_position, right_start);
        }
    }
    return result;
}

static Value parse_expression(Parser *parser)
{
    Value result = parse_term(parser);

    while (parser->error_position == 0) {
        size_t operator_position;
        size_t right_start;
        char operation;
        Value right;

        skip_spaces(parser);
        if (parser->position >= parser->line_end) {
            break;
        }
        operation = parser->text[parser->position];
        if (operation != '+' && operation != '-') {
            break;
        }
        operator_position = parser->position++;
        skip_spaces(parser);
        if (parser->position >= parser->line_end) {
            fail_at(parser, parser->line_end + 1);
            break;
        }
        right_start = parser->position;
        right = parse_term(parser);
        if (parser->error_position == 0) {
            result = apply_operator(parser, operation, result, right,
                                    operator_position, right_start);
        }
    }
    return result;
}

// AI (GitHub Copilot), prompt: "Handle the lab's large files and required command-line modes in standard C." Conversation history URL: unavailable in this workspace.
static int read_file(const char *path, char **contents, size_t *length)
{
    FILE *file = fopen(path, "rb");
    size_t used = 0;
    size_t capacity = 8192;
    char *buffer;

    if (file == NULL) {
        return 0;
    }
    buffer = malloc(capacity + 1);
    if (buffer == NULL) {
        fclose(file);
        errno = ENOMEM;
        return 0;
    }

    for (;;) {
        size_t available = capacity - used;
        size_t amount = fread(buffer + used, 1, available, file);
        used += amount;
        if (amount < available) {
            if (ferror(file)) {
                int saved_errno = errno == 0 ? EIO : errno;
                free(buffer);
                fclose(file);
                errno = saved_errno;
                return 0;
            }
            break;
        }
        if (used == capacity) {
            size_t new_capacity = capacity * 2;
            char *new_buffer = realloc(buffer, new_capacity + 1);
            if (new_buffer == NULL) {
                free(buffer);
                fclose(file);
                errno = ENOMEM;
                return 0;
            }
            buffer = new_buffer;
            capacity = new_capacity;
        }
    }
    fclose(file);
    buffer[used] = '\0';
    *contents = buffer;
    *length = used;
    return 1;
}

static int append_result(FILE *temporary, Value value)
{
    char output[128];
    int length;

    if (value.is_float) {
        length = snprintf(output, sizeof(output), "%.15g\n", value.real);
    } else {
        length = snprintf(output, sizeof(output), "%lld\n", value.integer);
    }
    return length > 0 && (size_t)length < sizeof(output) &&
           fwrite(output, 1, (size_t)length, temporary) == (size_t)length;
}

static int evaluate_file(const char *input_path, const char *output_path)
{
    char *contents;
    size_t length;
    size_t line_start = 0;
    FILE *temporary;
    FILE *output;
    size_t error_position = 0;
    int allocation_failed = 0;
    int failed = 0;

    if (!read_file(input_path, &contents, &length)) {
        fprintf(stderr, "calc: cannot read '%s': %s\n", input_path,
                strerror(errno));
        return 0;
    }
    temporary = tmpfile();
    if (temporary == NULL) {
        fprintf(stderr, "calc: cannot create temporary output: %s\n",
                strerror(errno));
        free(contents);
        return 0;
    }

    while (line_start < length) {
        size_t line_end = line_start;
        size_t first_nonblank;
        Parser parser;

        while (line_end < length && contents[line_end] != '\n') {
            line_end++;
        }
        first_nonblank = line_start;
        while (first_nonblank < line_end &&
               (contents[first_nonblank] == ' ' ||
                contents[first_nonblank] == '\t' ||
                contents[first_nonblank] == '\r')) {
            first_nonblank++;
        }

        if (first_nonblank < line_end && contents[first_nonblank] != '#') {
            Value value;

            parser.text = contents;
            parser.position = line_start;
            parser.line_end = line_end;
            parser.error_position = 0;
            parser.nesting = 0;
            parser.allocation_failed = 0;
            value = parse_expression(&parser);
            skip_spaces(&parser);
            if (parser.allocation_failed) {
                allocation_failed = 1;
                break;
            }
            if (parser.error_position == 0 && parser.position < line_end) {
                /* Any unconsumed token is extra syntax, reported where parsing stopped. */
                fail_at(&parser, parser.position + 1);
            }
            if (parser.error_position != 0) {
                error_position = parser.error_position;
                allocation_failed = parser.allocation_failed;
                break;
            }
            if (!append_result(temporary, value)) {
                fprintf(stderr, "calc: cannot buffer output for '%s'\n",
                        input_path);
                failed = 1;
                break;
            }
        }

        line_start = line_end < length ? line_end + 1 : length;
    }

    if (allocation_failed) {
        fprintf(stderr, "calc: out of memory while parsing '%s'\n", input_path);
        failed = 1;
    }
    free(contents);

    output = fopen(output_path, "wb");
    if (output == NULL) {
        fprintf(stderr, "calc: cannot write '%s': %s\n", output_path,
                strerror(errno));
        fclose(temporary);
        return 0;
    }
    if (error_position != 0) {
        if (fprintf(output, "ERROR:%zu\n", error_position) < 0) {
            failed = 1;
        }
    } else if (!failed) {
        char copy_buffer[8192];
        size_t amount;

        rewind(temporary);
        while ((amount = fread(copy_buffer, 1, sizeof(copy_buffer), temporary)) > 0) {
            if (fwrite(copy_buffer, 1, amount, output) != amount) {
                failed = 1;
                break;
            }
        }
        if (ferror(temporary)) {
            failed = 1;
        }
    }
    if (fclose(output) != 0) {
        failed = 1;
    }
    fclose(temporary);
    if (failed) {
        fprintf(stderr, "calc: output failed for '%s'\n", output_path);
        return 0;
    }
    return 1;
}

static char *duplicate_string(const char *text)
{
    size_t length = strlen(text);
    char *copy = malloc(length + 1);

    if (copy != NULL) {
        memcpy(copy, text, length + 1);
    }
    return copy;
}

static const char *base_name(const char *path)
{
    const char *end = path + strlen(path);
    const char *base;

    while (end > path && end[-1] == '/') {
        end--;
    }
    base = end;
    while (base > path && base[-1] != '/') {
        base--;
    }
    return base;
}

static char *without_txt_suffix(const char *name)
{
    size_t length = strlen(name);
    char *base = duplicate_string(name);

    if (base != NULL && length >= 4 && strcmp(name + length - 4, ".txt") == 0) {
        base[length - 4] = '\0';
    }
    return base;
}

static char *join_path(const char *directory, const char *name)
{
    size_t directory_length = strlen(directory);
    size_t name_length = strlen(name);
    int needs_separator = directory_length > 0 &&
                          directory[directory_length - 1] != '/';
    char *path = malloc(directory_length + (size_t)needs_separator +
                        name_length + 1);

    if (path == NULL) {
        return NULL;
    }
    memcpy(path, directory, directory_length);
    if (needs_separator) {
        path[directory_length++] = '/';
    }
    memcpy(path + directory_length, name, name_length + 1);
    return path;
}

static char *output_file_path(const char *output_directory,
                              const char *input_name)
{
    char *base = without_txt_suffix(base_name(input_name));
    char *filename;
    size_t needed;
    char *path;

    if (base == NULL) {
        return NULL;
    }
    needed = strlen(base) + strlen(STUDENT_FIRST) + strlen(STUDENT_LAST) +
             strlen(STUDENT_ID) + 8;
    filename = malloc(needed);
    if (filename == NULL) {
        free(base);
        return NULL;
    }
    (void)snprintf(filename, needed, "%s_%s_%s_%s.txt", base,
                   STUDENT_FIRST, STUDENT_LAST, STUDENT_ID);
    path = join_path(output_directory, filename);
    free(filename);
    free(base);
    return path;
}

static int ensure_output_directory(const char *path)
{
    struct stat status;

    if (stat(path, &status) == 0) {
        if (S_ISDIR(status.st_mode)) {
            return 1;
        }
        errno = ENOTDIR;
        return 0;
    }
    if (errno != ENOENT || mkdir(path, 0777) != 0) {
        return 0;
    }
    return 1;
}

static int append_string(StringList *list, const char *text)
{
    if (list->count == list->capacity) {
        size_t new_capacity = list->capacity == 0 ? 16 : list->capacity * 2;
        char **new_items = realloc(list->items, new_capacity * sizeof(*list->items));
        if (new_items == NULL) {
            return 0;
        }
        list->items = new_items;
        list->capacity = new_capacity;
    }
    list->items[list->count] = duplicate_string(text);
    if (list->items[list->count] == NULL) {
        return 0;
    }
    list->count++;
    return 1;
}

static int compare_strings(const void *left, const void *right)
{
    const char *const *left_string = left;
    const char *const *right_string = right;
    return strcmp(*left_string, *right_string);
}

static void free_string_list(StringList *list)
{
    size_t index;

    for (index = 0; index < list->count; index++) {
        free(list->items[index]);
    }
    free(list->items);
}

static int process_directory(const char *input_directory,
                             const char *output_directory)
{
    DIR *directory = opendir(input_directory);
    struct dirent *entry;
    StringList files = {NULL, 0, 0};
    size_t index;
    int success = 1;

    if (directory == NULL) {
        fprintf(stderr, "calc: cannot open directory '%s': %s\n",
                input_directory, strerror(errno));
        return 0;
    }
    while ((entry = readdir(directory)) != NULL) {
        size_t name_length = strlen(entry->d_name);
        char *candidate;
        struct stat status;

        if (name_length < 4 || strcmp(entry->d_name + name_length - 4, ".txt") != 0) {
            continue;
        }
        candidate = join_path(input_directory, entry->d_name);
        if (candidate == NULL) {
            success = 0;
            break;
        }
        if (stat(candidate, &status) == 0 && S_ISREG(status.st_mode) &&
            !append_string(&files, entry->d_name)) {
            success = 0;
        }
        free(candidate);
        if (!success) {
            break;
        }
    }
    closedir(directory);
    if (!success) {
        fprintf(stderr, "calc: out of memory while listing '%s'\n",
                input_directory);
        free_string_list(&files);
        return 0;
    }

    if (files.count > 1) {
        qsort(files.items, files.count, sizeof(*files.items), compare_strings);
    }
    for (index = 0; index < files.count; index++) {
        char *input_path = join_path(input_directory, files.items[index]);
        char *output_path = output_file_path(output_directory, files.items[index]);

        if (input_path == NULL || output_path == NULL) {
            fprintf(stderr, "calc: out of memory while building file path\n");
            success = 0;
        } else if (!evaluate_file(input_path, output_path)) {
            success = 0;
        }
        free(input_path);
        free(output_path);
        if (!success) {
            break;
        }
    }
    free_string_list(&files);
    return success;
}

static void print_usage(void)
{
    fprintf(stderr,
            "Usage: calc [-d DIR | --dir DIR] [-o OUTDIR | --output-dir OUTDIR] [input.txt]\n");
}

int main(int argc, char **argv)
{
    const char *input_directory = NULL;
    const char *output_directory = NULL;
    const char *input_file = NULL;
    char *default_output = NULL;
    int index;
    int success;

    for (index = 1; index < argc; index++) {
        if (strcmp(argv[index], "-d") == 0 || strcmp(argv[index], "--dir") == 0) {
            if (input_directory != NULL || index + 1 >= argc) {
                print_usage();
                return 2;
            }
            input_directory = argv[++index];
        } else if (strcmp(argv[index], "-o") == 0 ||
                   strcmp(argv[index], "--output-dir") == 0) {
            if (output_directory != NULL || index + 1 >= argc) {
                print_usage();
                return 2;
            }
            output_directory = argv[++index];
        } else if (argv[index][0] == '-') {
            print_usage();
            return 2;
        } else if (input_file == NULL) {
            input_file = argv[index];
        } else {
            print_usage();
            return 2;
        }
    }

    if ((input_directory == NULL && input_file == NULL) ||
        (input_directory != NULL && input_file != NULL)) {
        print_usage();
        return 2;
    }

    if (output_directory == NULL) {
        const char *base = input_directory != NULL
                               ? base_name(input_directory)
                               : base_name(input_file);
        char *output_base = input_directory != NULL
                                ? duplicate_string(base)
                                : without_txt_suffix(base);
        size_t needed;

        if (output_base == NULL) {
            fprintf(stderr, "calc: out of memory while building output directory\n");
            return 1;
        }
        needed = strlen(output_base) + strlen(STUDENT_FIRST) +
                 strlen(STUDENT_ID) + 3;
        default_output = malloc(needed);
        if (default_output != NULL) {
            (void)snprintf(default_output, needed, "%s_%s_%s", output_base,
                           STUDENT_FIRST, STUDENT_ID);
        }
        free(output_base);
        if (default_output == NULL) {
            fprintf(stderr, "calc: out of memory while building output directory\n");
            return 1;
        }
        output_directory = default_output;
    }

    if (!ensure_output_directory(output_directory)) {
        fprintf(stderr, "calc: cannot create output directory '%s': %s\n",
                output_directory, strerror(errno));
        free(default_output);
        return 1;
    }

    if (input_directory != NULL) {
        struct stat status;
        if (stat(input_directory, &status) != 0 || !S_ISDIR(status.st_mode)) {
            fprintf(stderr, "calc: input directory '%s' does not exist or is not a directory\n",
                    input_directory);
            success = 0;
        } else {
            success = process_directory(input_directory, output_directory);
        }
    } else {
        char *output_path = output_file_path(output_directory, input_file);
        if (output_path == NULL) {
            fprintf(stderr, "calc: out of memory while building output file path\n");
            success = 0;
        } else {
            success = evaluate_file(input_file, output_path);
        }
        free(output_path);
    }

    free(default_output);
    return success ? 0 : 1;
}