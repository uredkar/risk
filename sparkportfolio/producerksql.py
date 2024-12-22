from kafka import KafkaProducer

# Configure Kafka Producer
bootstrap_servers = ['localhost:9092']  # Replace with your Kafka broker(s)
topic_name = 'my_input_topic'

producer = KafkaProducer(bootstrap_servers=bootstrap_servers)

# Sample data (replace with your actual data)
data = [
    {'user_id': 1, 'event_type': 'login', 'timestamp': '2024-11-21T10:00:00'},
    {'user_id': 2, 'event_type': 'purchase', 'timestamp': '2024-11-21T10:30:00'},
    {'user_id': 1, 'event_type': 'logout', 'timestamp': '2024-11-21T11:00:00'}
]

for record in data:
    key = str(record['user_id']).encode('utf-8')  # Example: use user_id as key
    value = str(record).encode('utf-8')
    producer.send(topic_name, key=key, value=value)

producer.flush()  # Ensure all messages are sent
producer.close()

print(f"Produced {len(data)} messages to topic '{topic_name}'")