# rslang-service

## Running locally

The web service application is published as a [Docker image](https://hub.docker.com/r/daquirm/rslang-service).
In order to store and retrieve data it needs a running [postgres](https://www.postgresql.org/) container.

1. Install [Docker](https://www.docker.com/) and make sure it's running.
2. Create a Docker network to be used by the containers: `docker network create rslang`.
3. Pull and run postgres: `docker run -e POSTGRES_USER=rslang-service -e POSTGRES_PASSWORD=rslang -e POSTGRES_DB=lang -v postgres-data:/var/lib/postgresql/data --network=rslang --network-alias=pg -p 5432:5432 -d postgres`

It's a good idea to install a visual database administration tool such as [pgAdmin](https://www.pgadmin.org/) to browse and manipulate the service database.

4. Pull and run the service app: `docker run -p 3000:3000 --network=rslang daquirm/rslang-service`
5. Verify that the service app is responding by sending a request to an endpoint (for example `GET /users`)

To send HTTP requests use `curl` or install a REST API client, such as [Advanced REST Client](https://install.advancedrestclient.com/install).

6. The service app can initialise the database by creating all the schemas/tables. Send `POST /schema/init`.
7. Your personal rslang backend is ready to use! Now you can use [Swagger Editor](editor.swagger.io) to view the API documentation. Get the JSON data (`GET /swagger.json`) and copy-and-paste it into the editor.
