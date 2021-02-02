use iron::mime::*;
use iron::prelude::*;
use iron::status;
use pulldown_cmark::{html, Options, Parser};
use router::Router;
use std::collections::hash_map::RandomState;
use std::collections::HashMap;
use std::io::Error;
use urlencoded::{UrlDecodingError, UrlEncodedQuery};

fn view_file(request: &mut Request) -> IronResult<Response> {
    let mut response = Response::new();

    let result = match request.get_ref::<UrlEncodedQuery>() {
        Ok(map) => map,
        Err(e) => {
            response.set_mut(status::BadRequest);
            return Ok(response);
        }
    };
    let path = match result.get("path") {
        None => {
            response.set_mut(status::BadRequest);
            return Ok(response);
        }
        Some(nums) => nums,
    };

    // TODO: ensure that the path does not go outside the working directory
    let content = match std::fs::read_to_string(&path[0]) {
        Ok(c) => c,
        Err(_) => {
            response.set_mut(status::BadRequest);
            return Ok(response);
        }
    };

    let mut rendered = String::new();
    let parser = Parser::new(&content);
    html::push_html(&mut rendered, parser);

    response.set_mut(status::Ok);
    response.set_mut(mime!(Text/Html; Charset=Utf8));
    response.set_mut(rendered);

    Ok(response)
}

fn main() {
    let mut router = Router::new();
    router.get("/view", view_file, "root");
    Iron::new(router).http("localhost:31337").unwrap();
}
