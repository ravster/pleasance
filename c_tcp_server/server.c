#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <arpa/inet.h>

// gcc server.c -o run -lpthread
// Test with
// echo "hghghg" | nc localhost 3003

#define PORT 3003

// This function is run in a thread, to handle the request.
void*
handle_request(void* client_socket_ptr) {
	int client_socket = *((int*) client_socket_ptr);
	free(client_socket_ptr);
	char buf[1024];
	int bytes_read = read(client_socket, buf, 1023);

	if (bytes_read > 0) {
		buf[bytes_read] = 0;
		printf("Received %s\n", buf);
		char* out = malloc(256);
		sprintf(out, "This is thread %p.\nYou said %s.\nMy response is %p",
				(void*)pthread_self(),
				buf,
				&out);
		write(client_socket, out, strlen(out));
	}

	close(client_socket);
	return NULL;
}

int
main() {
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

	while (1) {
		struct sockaddr_in client_addr;
		socklen_t addr_len = sizeof(client_addr);

		int* new_socket = malloc(sizeof(int));
		// In happy path, new_socket is freed by the worker thread.
		*new_socket = accept(server_fd, (struct sockaddr*)&client_addr, &addr_len);
		if (*new_socket < 0) {
			perror("accept failed");
			free(new_socket);
			continue;
		}

		// Make new thread to handle request.
		pthread_t thread_id;
		int err = 0;
		err = pthread_create(&thread_id, NULL, handle_request, (void*)new_socket);
		if (err != 0) {
			perror("Couldn't create thread");
			close(*new_socket);
			free(new_socket);
		} else {
			printf("Fired off thread\n");
			// We don't want it to be a zombie thread after it's done working.
			pthread_detach(thread_id);
		}
	}
}
