using GrpcServiceDemo;
using GrpcServiceDemo.Services;
using Microsoft.IdentityModel.Tokens;
using System.Text;


var builder = WebApplication.CreateBuilder(args);

// Add services to the container.
//builder.Services.AddGrpc();
builder.Services.AddGrpc(options =>
{
    //options.AddInterceptor<LoggingInterceptor>();
    options.Interceptors.Add<LoggingInterceptor>();

});
builder.Services.AddAuthentication("Bearer")
    .AddJwtBearer("Bearer", options =>
    {
        options.TokenValidationParameters = new TokenValidationParameters
        {
            ValidateIssuer = true,
            ValidateAudience = true,
            ValidateLifetime = true,
            ValidateIssuerSigningKey = true,
            ValidIssuer = "mock-issuer", // Match the mock issuer
            ValidAudience = "mock-audience", // Match the mock audience
            IssuerSigningKey = new SymmetricSecurityKey(Encoding.UTF8.GetBytes("your-secret-key-12345")) // Match the secret key
        };
    });

builder.Services.AddAuthorization();

var app = builder.Build();



app.UseAuthentication();
app.UseAuthorization();

// Configure the HTTP request pipeline.
app.MapGrpcService<GreeterService>();
app.MapGrpcService<HftServiceImpl>();

app.MapGet("/", () => "Communication with gRPC endpoints must be made through a gRPC client. To learn how to create a client, visit: https://go.microsoft.com/fwlink/?linkid=2086909");
app.MapPost("/generate-token", (HttpContext context) =>
{
    
    Console.WriteLine($"body {context.Request.Body}");
    

    var username = "username";
    Console.WriteLine($"username {username}");
    var secretKey = "12345678901234567890123456789021";
    var token = JwtTokenGenerator.GenerateToken(secretKey, "mock-issuer", "mock-audience");
    return Results.Ok(new { token });
});

app.Run();
