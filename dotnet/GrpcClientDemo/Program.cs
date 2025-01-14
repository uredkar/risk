using Grpc.Core;
using Grpc.Net.Client;
using GrpcServiceDemo;
using System.Net.Http.Json;
using System.Net.Security;
using System.Security.Cryptography.X509Certificates;
using System.Text.Json;
using System.Text;
using System.Net.Http.Headers;
using GrpcClientDemo;
var username = "testuser";

var token = await GetTokenAsync(username);
Console.WriteLine($"token {token}");

var headers = new Metadata();
headers.Add("Authorization", $"Bearer {token}");

using var channel = GrpcChannel.ForAddress("https://localhost:5001");
//var client = new Greeter.GreeterClient(channel);

//var reply = await client.SayHelloAsync(new HelloRequest { Name = "World" }, headers);
//Console.WriteLine($"Greeting: {reply.Message}");

await HftClient.Execute();

async Task<string> GetTokenAsync(string username)
{
  

    using var handler = new HttpClientHandler
    {
        ServerCertificateCustomValidationCallback = (sender, certificate, chain, sslPolicyErrors) =>
        {
            // This is a simplified example. 
            // You should implement more robust certificate validation logic here.
            if (sslPolicyErrors == SslPolicyErrors.None)
            {
                Console.WriteLine("Cert is valid");
                return true; // Certificate is valid
            }

            // Handle specific errors (e.g., certificate expired, invalid chain)
            // For example:
            if (sslPolicyErrors == SslPolicyErrors.RemoteCertificateChainErrors)
            {
                Console.WriteLine("Cert is NOT valid");
                // Log or handle the certificate chain error
                return true; // Allow the request to proceed (not recommended in production)
            }

            return false; // Certificate is invalid
        }
    };

    using var handler2 = new HttpClientHandler
    {
        ServerCertificateCustomValidationCallback = (sender, certificate, chain, sslPolicyErrors) => true
    };

    if (string.IsNullOrEmpty(username))
    {
        throw new ArgumentNullException(nameof(username), "Username cannot be null or empty.");
    }

    using var httpClient = new HttpClient(handler2);
    // Create a JSON content object with the username
    //var jsonContent = JsonContent.Create(new { Username = username });
    var user = new
    {
        Username = "john.doe",
        // Add other user properties here
    };

    // Serialize the user object to JSON
    string json = JsonSerializer.Serialize(user);

    // Create a StringContent object with the JSON data
    var jsonContent = new StringContent(json, Encoding.UTF8, "application/json");
    httpClient.DefaultRequestHeaders.Accept.Clear();
    httpClient.DefaultRequestHeaders.Accept.Add(
        new MediaTypeWithQualityHeaderValue("application/json"));


    var response = await httpClient.PostAsync("https://localhost:5001/generate-token", jsonContent);
    response.EnsureSuccessStatusCode();

    var jsonResponse = await response.Content.ReadFromJsonAsync<TokenResponse>();
    return jsonResponse?.Token ?? throw new Exception("Failed to retrieve token");
}

record TokenResponse(string Token);



