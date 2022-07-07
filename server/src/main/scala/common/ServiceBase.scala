package com.sheepduke.silver_brain
package common

enum ServiceError {
  case NotFound, BadRequest
}

type ServiceResponse[A] = Either[ServiceError, A]
