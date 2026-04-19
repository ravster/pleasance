#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <pthread.h>
#include <arpa/inet.h>

// gcc client.c -o client -lpthread

#define PORT 3003
#define NUM_THREADS 20

// This function is run in a thread.
void*
handle_connection(void* thread_id_ptr) {
	int thread_id = *(int*)thread_id_ptr;
	int sock = 0;
	struct sockaddr_in serv_addr;

	// Create socket
	sock = socket(AF_INET, SOCK_STREAM, 0);

	serv_addr.sin_family = AF_INET;
	serv_addr.sin_port = htons(3003);
	inet_pton(AF_INET, "127.0.0.1", &serv_addr.sin_addr);

	// Connect
	connect(sock, (struct sockaddr*)&serv_addr, sizeof(serv_addr));

	// Send msg
	char msg[30] = {0};
	sprintf(msg, "Msg from thread %d.\t%d", thread_id, thread_id);
	send(sock, msg, strlen(msg), 0);

	// Rcv response
	char buf[1024] = {0};
	int valread = read(sock, buf, 29);
	if (valread > 0) {
		printf("Thread %d got resp:\n->\t%s\n\n", thread_id, buf);
	}

	close(sock);
	return NULL;
}

int
main() {
	pthread_t threads[NUM_THREADS];
	int thread_ids[NUM_THREADS];
	printf("Starting %d threads\n", NUM_THREADS);

	for (int i = 0; i < NUM_THREADS; ++i) {
		thread_ids[i] = i+1;
		pthread_create(&threads[i], NULL, handle_connection, &thread_ids[i]);
	}
	for (int i = 0; i < NUM_THREADS; ++i) {
		pthread_join(threads[i], NULL);
	}

	printf("All threads done\n");
	return 0;
}
