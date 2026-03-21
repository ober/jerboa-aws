/*
 * jerboa-aws-main.c — Custom entry point for jerboa-aws.
 *
 * Boot files (petite.boot, scheme.boot, app.boot) are embedded as C byte
 * arrays and registered via Sregister_boot_file_bytes.
 *
 * Threading workaround: Programs in boot files cannot create threads
 * (Chez bug). The program is loaded separately via Sscheme_script on a memfd.
 */

#define _GNU_SOURCE
#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <unistd.h>
#include <sys/mman.h>
#include "scheme.h"
#include "jerboa_aws_program.h"
#include "jerboa_aws_petite_boot.h"
#include "jerboa_aws_scheme_boot.h"
#include "jerboa_aws_app_boot.h"

int main(int argc, char *argv[]) {
    char countbuf[32];
    snprintf(countbuf, sizeof(countbuf), "%d", argc - 1);
    setenv("AWS_ARGC", countbuf, 1);

    for (int i = 1; i < argc; i++) {
        char name[32];
        snprintf(name, sizeof(name), "AWS_ARG%d", i - 1);
        setenv(name, argv[i], 1);
    }

    int fd = memfd_create("jerboa-aws-program", MFD_CLOEXEC);
    if (fd < 0) { perror("memfd_create"); return 1; }
    if (write(fd, jerboa_aws_program_data, jerboa_aws_program_size) != (ssize_t)jerboa_aws_program_size) {
        perror("write memfd"); close(fd); return 1;
    }
    char prog_path[64];
    snprintf(prog_path, sizeof(prog_path), "/proc/self/fd/%d", fd);

    Sscheme_init(NULL);
    Sregister_boot_file_bytes("petite", (void*)petite_boot_data, petite_boot_size);
    Sregister_boot_file_bytes("scheme", (void*)scheme_boot_data, scheme_boot_size);
    Sregister_boot_file_bytes("app", (void*)jerboa_aws_app_boot_data, jerboa_aws_app_boot_size);

    Sbuild_heap(NULL, NULL);
    const char *script_args[] = { argv[0] };
    int status = Sscheme_script(prog_path, 1, script_args);

    close(fd);
    Sscheme_deinit();
    return status;
}
