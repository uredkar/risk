from ksqldb import KSQLdbClient

# Connect to the KSQL server
client = KSQLdbClient('http://localhost:8088') 

# Execute a KSQL query
query = "CREATE STREAM user_activity_stream (user_id INT, event_type VARCHAR, timestamp TIMESTAMP) WITH (kafka_topic='user_activity', value_format='JSON');"
client.ksql(query)

# Execute another KSQL query (e.g., to query data)
query = "SELECT * FROM user_activity_stream LIMIT 10;"
results = client.query(query)

# Print the results
for row in results:
    print(row)

# Close the connection
client.close()