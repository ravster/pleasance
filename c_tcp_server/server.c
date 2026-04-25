#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <arpa/inet.h>

// gcc server.c -o run -lpthread
// Test with
// echo "hghghg" | nc localhost 3003
// or better yet, use the client program in this dir.

#define PORT 3003

/*
 * Returns the number of splits.
 * Writes into the out param, which is already malloc'd.
 */
int
string_split(char* in, char** out, char* delim, int max_count) {
	char* saveptr;
	int count = 0;
	char* substring = strtok_r(in, delim, &saveptr);

	while (substring != NULL) {
		printf("in loop. substring=%s\n", substring);
		out[count] = substring;
		++count;
		if (count == max_count) {
			// We allocated only this much.
			break;
		}
		substring = strtok_r(NULL, delim, &saveptr);
	}
	printf("count %d\n", count);
	return count;
}

// This function is run in a thread, to handle the request.
void*
handle_request(void* client_socket_ptr) {
	int client_socket = *((int*) client_socket_ptr);
	free(client_socket_ptr);
	char buf[1024];
	int bytes_read = read(client_socket, buf, 1023);
	buf[bytes_read] = 0;

	if (bytes_read > 0) {
		printf("Received %s\n", buf);

		// String split
		char** strings = calloc(20, sizeof(char*));
		int string_count = 0;
		string_count = string_split(buf, strings, "\t", 20);

		if (string_count < 2) {
			char* errmsg;
			asprintf(&errmsg, "Invalid string count:%d from string:%s\n", string_count, buf);
			fprintf(stderr, "%s", errmsg);
			dprintf(client_socket, "%s", errmsg);
			free(errmsg);
			close(client_socket);
			return NULL;
		}
		char* first = strings[0];
		char* second = strings[1];
		int code = atoi(second);

		char* resp = malloc(32);
		switch (code) {
			case 1:
				resp = "foo";
				break;
			case 2:
				resp = "bar";
				break;
			default:
				resp = "baz";
		}
		printf("1: %s\n2: %s\nResult: %s\n", first, second, resp);

		// Response
		char* out = malloc(256);
		out = resp;
		write(client_socket, out, strlen(out));
	}

	close(client_socket);
	return NULL;
}

int
listen_on_port() {
	int server_fd;
	struct sockaddr_in address;
	int opt = 1;
	server_fd = socket(AF_INET, SOCK_STREAM, 0);

	address.sin_family = AF_INET;
	address.sin_addr.s_addr = INADDR_ANY;
	address.sin_port = htons(PORT);

	bind(server_fd, (struct sockaddr*)&address, sizeof(address));
	listen(server_fd, 100);
	printf("Server listening on port %d...\n", PORT);

	return server_fd;
}

int
main() {
	int server_fd = listen_on_port();
	while (1) {
		// Build up client socket.
		struct sockaddr_in client_addr;
		socklen_t addr_len = sizeof(client_addr);
		int* new_socket = malloc(sizeof(int));
		// This blocks till a connection comes through. Easy.
		// In happy path, new_socket is freed by the worker thread.
		*new_socket = accept(server_fd, (struct sockaddr*)&client_addr, &addr_len);
		if (*new_socket < 0) {
			perror("accept failed");
			free(new_socket);
			continue;
		}

		// TODO: Instead of making a new OS thread, make a thread-pool.
		// Create a queue.
		// Then make 4-8 threads and have them read from the queue.
		// When a new client-socket is made, push that into the queue.
		// Figure out the pthreads way to wake all or some of the threads in the pool.
		// Make new thread to handle request.
		pthread_t thread_id;
		int err = 0;
		err = pthread_create(&thread_id, NULL, handle_request, (void*)new_socket);
		if (err != 0) {
			perror("Couldn't create thread");
			close(*new_socket);
			free(new_socket);
		} else {
			// Thread successfully made. We don't want it to be a zombie after it's done
			// work, so set it to clean up after it finishes execution.
			pthread_detach(thread_id);
		}
	}
}
