from confluent_kafka import Consumer, KafkaError

def main():
    # Kafka consumer configuration
    conf = {
        'bootstrap.servers': '127.0.0.1:9092',  # Update with the appropriate listener (e.g., host.docker.internal:29092)
        'group.id': 'python-consumer-group',
        'auto.offset.reset': 'earliest',  # Start reading from the beginning of the topic
    }

    # Create a Kafka consumer
    consumer = Consumer(conf)

    # Subscribe to a topic
    topic = 'my-topic'  # Replace with your topic
    consumer.subscribe([topic])

    try:
        print(f"Subscribed to topic: {topic}")
        while True:
            # Poll for new messages
            msg = consumer.poll(1.0)  # Timeout of 1 second

            if msg is None:
                continue  # No message received, continue polling
            if msg.error():
                # Handle errors
                if msg.error().code() == KafkaError._PARTITION_EOF:
                    # End of partition event
                    print(f"Reached end of partition: {msg.topic()} [{msg.partition()}]")
                else:
                    print(f"Error: {msg.error()}")
                continue

            # Print the received message
            print(f"Received message: {msg.value().decode('utf-8')} (key: {msg.key()})")

    except KeyboardInterrupt:
        print("Consumer stopped.")

    finally:
        # Close the consumer
        consumer.close()

if __name__ == "__main__":
    main()
